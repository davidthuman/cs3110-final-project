open Async
open Async_unix
open ANSITerminal
open Server

let server = ref (Server.init_server ())
let parse = Server.parse

(* [connection_reader addr r w] is a resursive function that completes
   async actions. This function is constantly waiting for a client to
   write to its Reader.t. Once something is written, connection_reader
   reads the string, parses it for a 5 bit code, does that code's
   asccociated action, and then calls itself with the same arguments.*)
let rec connection_reader r w =
  print_endline "New Client";
  let rec loop read write =
    Reader.read_line read >>= function
    | `Eof ->
        print_endline "Error: reading from server\n";
        return ()
    | `Ok line ->
        let code = int_of_string (String.sub line 0 5) in
        let input = String.sub line 5 (String.length line - 5) in
        print_endline
          ("Received: code: " ^ string_of_int code ^ ", input: " ^ input);
        let new_server = { !server with code; input } in
        server := parse new_server r w;
        Writer.write_line w !server.response;
        loop r w
  in

  loop r w

let create_tcp port =
  let host_and_port =
    Async.Tcp.Server.create ~on_handler_error:`Raise
      (Async.Tcp.Where_to_listen.of_port port) (fun addr r w ->
        connection_reader r w)
  in
  ignore
    (host_and_port
      : (Socket.Address.Inet.t, int) Async.Tcp.Server.t Deferred.t);
  Deferred.never ()

let run port =
  ignore (create_tcp port);
  Deferred.never ()

let reg = [ Foreground White ]

let main () =
  print_endline "Starting the chat server ... ";
  print_endline "Server is running";
  Command.async ~summary:"" (Command.Param.return (fun () -> run 9999))
  |> Command.run

let () = main ()