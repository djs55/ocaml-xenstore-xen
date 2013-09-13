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

let version = "1.9.9"

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

let default_pidfile = "/var/run/xenstored.pid"

open Cmdliner

let pidfile =
  let doc = "The path to the pidfile, if running as a daemon" in
  Arg.(value & opt string default_pidfile & info [ "pidfile" ] ~docv:"PIDFILE" ~doc)

let daemon =
  let doc = "Run as a daemon" in
  Arg.(value & flag & info [ "daemon" ] ~docv:"DAEMON" ~doc)

let path =
  let doc = "The path to the Unix domain socket" in
  Arg.(value & opt string !Xs_transport_unix.xenstored_socket & info [ "path" ] ~docv:"PATH" ~doc)

let enable_xen =
  let doc = "Provide service to VMs over shared memory" in
  Arg.(value & flag & info [ "enable-xen" ] ~docv:"XEN" ~doc)

let enable_unix =
  let doc = "Provide service locally over a Unix domain socket" in
  Arg.(value & flag & info [ "enable-unix" ] ~docv:"UNIX" ~doc)

let program_thread daemon pidfile enable_xen enable_unix =

  debug "User-space xenstored version %s starting" version;
  let (_: 'a) = logging_thread Xenstore_server.Logging.logger in
  let (_: 'a) = logging_thread Xenstore_server.Logging.access_logger in

  lwt () = if daemon then begin
    debug "Writing pidfile %s" pidfile;
    (try Unix.unlink pidfile with _ -> ());
    let pid = Unix.getpid () in
    lwt _ = Lwt_io.with_file pidfile ~mode:Lwt_io.output (fun chan -> Lwt_io.fprintlf chan "%d" pid) in
    return ()
  end else begin
    debug "We are not daemonising so no need for a pidfile.";
    return ()
  end in
  let (a: unit Lwt.t) =
    if enable_unix then begin
      debug "Starting server on unix domain socket %s" !Xs_transport_unix.xenstored_socket;
      UnixServer.serve_forever ()
    end else return () in
  let (b: unit Lwt.t) =
    if enable_xen then begin
      debug "Starting server on xen inter-domain transport";
      DomainServer.serve_forever ()
    end else return () in
  Xenstore_server.Introduce.(introduce { domid = 0; mfn = 0n; remote_port = 0 });
  debug "Introduced domain 0";
  lwt () = a in
  lwt () = b in
  debug "No running transports, shutting down.";
  return ()

let program pidfile daemon path enable_xen enable_unix =
  if enable_unix then begin
    let dir_needed = Filename.dirname path in
    if not(Sys.file_exists dir_needed && (Sys.is_directory dir_needed)) then begin
      Printf.fprintf stderr "The directory where the socket should be created (%s) doesn't exist.\n" dir_needed;
      exit 1;
    end
  end;
  Xs_transport_unix.xenstored_socket := path;
  if daemon then Lwt_daemon.daemonize ();
  try
    Lwt_main.run (program_thread daemon pidfile enable_xen enable_unix)
  with e ->
    exit 1

let program_t = Term.(pure program $ pidfile $ daemon $ path $ enable_xen $ enable_unix)

let info =
  let doc = "User-space xenstore server" in
  let man = [
    `S "DESCRIPTION";
    `P "The xenstore service allows Virtual Machines running on top of the Xen hypervisor to share configuration information and setup high-bandwidth shared-memory communication channels for disk and network IO.";
    `P "The xenstore service provides a tree of key=value pairs which may be transactionally updated over a simple wire protocol. Traditionally the service exposes the protocol both over a Unix domain socket (for convenience in domain zero) and over shared memory rings. Note that it is also possible to run xenstore as a xen kernel, for enhanced isolation: see the ocaml-xenstore-xen/xen frontend.";
    `S "EXAMPLES";
    `P "To run as the main xenstore service on a xen host:";
    `P "  $(tname) --daemon --enable-xen --enable-unix";
    `P "To run in userspace only in the foreground for testing on an arbitrary host:";
    `P "  $(tname) --enable-unix --path ./mysocket";
    `S "BUGS";
    `P "Please report bugs at https://github.com/xapi-project/ocaml-xenstore-xen"
  ] in
  Term.info "xenstored" ~version ~doc ~man

let () = match Term.eval (program_t, info) with
  | `Ok () -> exit 0
  | _ -> exit 1

