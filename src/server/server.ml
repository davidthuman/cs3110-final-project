open Core
open Async

(* [user_data] is the list of (unique usernames) and passwords *)
type user_data = (string * string) list

(* [current_conn] is the list of (unique usernames) attached to the
   users (Reader and Writer) pair. *)
type current_conn = (string * (Reader.t * Writer.t)) list

(* [chat] is the list of (unique integers) to represent different caht
   rooms attached to a string list of (unique usernames) of user that
   are currently in that chatroom. *)
type chat = (int * string list) list

(* [msg_hist] is the list of messages that have been sent in a certain
   chatroom. Each (unique integer) chatroom with have attached a string
   tuple representing a (unique username) and their message. *)
type msg_hist = (int * (string * string) list) list

(* [state] is the current state of the total data that the server has
   stored. *)
type state = {
  user_data : user_data;
  current_conn : current_conn;
  chat : chat;
  msg_hist : msg_hist;
}

(* [server_data] is the current data collection that the server is
   attempting to handle. *)
type server_data = {
  state : state;
  username : string;
  chatid : int;
  code : int;
  input : string;
  response : string;
}

let insert lst e = e :: lst

(* [init_state] initializes the state of the total data that the server
   has stored. To start, there is one chatroom, with chatid = 0, which
   represents the lobby or default chatroom. *)
let init_state () =
  {
    user_data = [];
    current_conn = [];
    chat = (0, []) |> insert [];
    msg_hist = [];
  }

(* [init_server] initializes the current data to be processed by the
   server. *)
let init_server () =
  {
    state = init_state ();
    username = "";
    chatid = 0;
    code = 0;
    input = "";
    response = "";
  }

(* [add_to_chat chat chatroom username] returns a chat type with the new
   username added to the user list for the given chatroom. *)
let add_to_chat chat chatroom username =
  match (chat, chatroom) with
  | [ (1, l1); (0, l0) ], 0 -> [ (0, username :: l0); (1, l1) ]
  | [ (0, l0); (1, l1) ], 1 -> [ (1, username :: l1); (0, l0) ]
  | _ -> chat

let get_username server_data reader =
  let state = server_data.state in
  let currconn = state.current_conn in
  let rec check lst =
    match lst with
    | [] -> "NONE"
    | (username, (r, w)) :: t ->
        if r == reader then username else check t
  in
  check currconn

let get_chat server_data username =
  let state = server_data.state in
  let chat = state.chat in
  let rec check lst =
    match lst with
    | [ (1, l1); (0, l0) ] ->
        if List.mem l0 username (fun x y -> String.compare x y = 0) then
          0
        else 1
    | _ -> 0
  in
  check chat

(* [check_username user_data username] returns true if the [username] is
   not in the [user_data] list, and false if it is. *)
let rec check_username user_data username =
  match user_data with
  | [] ->
      print_endline "Server: Username not found";
      true
  | (u, _) :: t ->
      print_endline ("check username : " ^ u);
      if String.compare u username = 0 then false
      else check_username t username

(* [login server_data line r w] will check to see if a given username in
   [line] is found in the server_data and will respond with "false" if
   it is not or "UNAME_EXISTS" if it does.*)
let login server_data line r w =
  if check_username server_data.state.user_data line then (
    print_endline "Server: Failed Log In";
    { server_data with response = "false" })
  else
    {
      server_data with
      state =
        {
          server_data.state with
          current_conn =
            (line, (r, w)) :: server_data.state.current_conn;
          chat = add_to_chat server_data.state.chat 0 line;
        };
      username = line;
      response = "UNAME_EXISTS";
    }

(* [signup server_data line r w] check if a given username in [line] is
   already used, if it is it will return "UNAME_EXISTS" or if not
   "true", then it will update the server_data to add a new user with
   username [line] and a blank password. *)
let signup server_data line r w =
  if check_username server_data.state.user_data line then
    {
      server_data with
      state =
        {
          server_data.state with
          user_data = (line, "") :: server_data.state.user_data;
          current_conn =
            (line, (r, w)) :: server_data.state.current_conn;
          chat = add_to_chat server_data.state.chat 0 line;
        };
      username = line;
      response = "true";
    }
  else { server_data with response = "UNAME_EXIST" }

(* [check_password uname pass] returns true if the given
   username-password pair at in the server state, and false otherwise.*)
let rec check_password user_data uname pass =
  match user_data with
  | [] -> false
  | (u, p) :: t ->
      if String.compare u uname = 0 && String.compare p pass = 0 then
        true
      else (
        print_endline
          (uname ^ " - " ^ pass ^ " does not match " ^ u ^ " - " ^ p);
        check_password t uname pass)

(* [change_pass uname pass wr] changes the password of the client
   associated with Writer.t wr.*)
let change_pass user_data uname pass =
  let rec through lst acc =
    match lst with
    | [] -> acc
    | (u, p) :: t ->
        if String.compare u uname = 0 then (
          print_endline ("New user data: (" ^ uname ^ ", " ^ pass ^ ")");
          (uname, pass) :: (acc @ t))
        else through t ((u, p) :: acc)
  in
  through user_data []

let get_username_from_chat chat chatid =
  match (chat, chatid) with
  | [ (1, l1); (0, l0) ], 0 -> l0
  | [ (0, l0); (1, l1) ], 1 -> l1
  | _ -> []

let send_message server_data line =
  let chat, chatid, u0, currconn =
    ( server_data.state.chat,
      server_data.chatid,
      server_data.username,
      server_data.state.current_conn )
  in
  let users, mess =
    (get_username_from_chat chat chatid, u0 ^ ": " ^ line)
  in

  print_endline ("Message: " ^ mess);
  let rec message chatuser all_conn =
    match all_conn with
    | [] -> server_data
    | (user, (r, w)) :: t ->
        Writer.write_line w mess;
        print_endline "Message sent";
        message chatuser t
  in
  message users currconn

(* [parse line] is the string response that will be written to the
   client.*)
let parse server_data r w =
  let sys = server_data.code in
  let user_input = server_data.input in
  match sys with
  | 00001 ->
      print_endline "Loginning in user";
      login server_data user_input r w
  (* Code 00010 is for sign up *)
  | 00010 ->
      print_endline "Signing up user";
      signup server_data user_input r w
  (* Code 00011 is for sending message*)
  | 00011 ->
      print_endline "Sending message";
      send_message server_data user_input
  (* Code 00100 is for checking to see if a username - password pair
     matches*)
  | 00100 ->
      print_endline "Checking password";
      if
        check_password server_data.state.user_data server_data.username
          user_input
      then
        {
          server_data with
          state =
            {
              server_data.state with
              current_conn =
                (server_data.username, (r, w))
                :: server_data.state.current_conn;
              chat =
                add_to_chat server_data.state.chat 0
                  server_data.username;
            };
          response = "true";
        }
      else { server_data with response = "false" }
  (* Code 00101 is for changing the password for a username*)
  | 00101 ->
      print_endline "Changing password";
      {
        server_data with
        state =
          {
            server_data.state with
            user_data =
              change_pass server_data.state.user_data
                server_data.username user_input;
          };
        chatid = 0;
        response = "true";
      }
  | _ -> server_data
