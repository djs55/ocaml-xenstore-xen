(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Lwt
open Xs_protocol

let debug fmt = Xenstore_server.Logging.debug "server_unix" fmt

module UnixServer = Xenstore_server.Xs_server.Server(Xs_transport_unix)
module DomainServer = Xenstore_server.Xs_server.Server(Xs_transport_xen)

let syslog = Lwt_log.syslog ~facility:`Local3 ()

let daemon = ref false

let rec logging_thread logger =
  lwt lines = Xenstore_server.Logging.get logger in
  lwt () = Lwt_list.iter_s
    (fun x ->
      lwt () =
        if !daemon
        then Lwt_log.log ~logger:syslog ~level:Lwt_log.Notice x
        else Lwt_io.write_line Lwt_io.stdout x in
        return ()
    ) lines in
  logging_thread logger

let pidfile = ref "/var/run/xenstored.pid"

let main () =
  debug "Unix xenstored starting";
  let (_: 'a) = logging_thread Xenstore_server.Logging.logger in
  let (_: 'a) = logging_thread Xenstore_server.Logging.access_logger in

  debug "Writing pidfile %s" !pidfile;
  (try Unix.unlink !pidfile with _ -> ());
  let pid = Unix.getpid () in
  lwt _ = Lwt_io.with_file !pidfile ~mode:Lwt_io.output (fun chan -> Lwt_io.fprintlf chan "%d" pid) in

  let (a: unit Lwt.t) = UnixServer.serve_forever () in
  debug "Started server on unix domain socket";
  let (b: unit Lwt.t) = DomainServer.serve_forever () in
  debug "Started server on xen inter-domain transport";
  Xenstore_server.Introduce.(introduce { domid = 0; mfn = 0n; remote_port = 0 });
  debug "Introduced domain 0";
  lwt () = a in
  lwt () = b in
  return ()

let _ =
  Arg.parse [
    "-path", Arg.Set_string Xs_transport_unix.xenstored_socket, Printf.sprintf "Unix domain socket to listen on (default %s)" !Xs_transport_unix.xenstored_socket;
    "-pidfile", Arg.Set_string pidfile, Printf.sprintf "File to write PID (default %s)" !pidfile;
    "-daemon", Arg.Set daemon, Printf.sprintf "True if we should daemonize (default %b)" !daemon;
   ]
  (fun _ -> ())
  "User-space xenstore service";
  let dir_needed = Filename.dirname !Xs_transport_unix.xenstored_socket in
  if not(Sys.file_exists dir_needed && (Sys.is_directory dir_needed)) then begin
    Printf.fprintf stderr "The directory where the socket should be created (%s) doesn't exist.\n" dir_needed;
    exit 1;
  end;
  if !daemon then Lwt_daemon.daemonize ();
  Lwt_main.run (main ())
