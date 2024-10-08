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

(* [state] is the state of the total data that the server has stored. *)
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

(* [init_state] is the initial state of the current_state. *)
val init_state : unit -> state

(* [init_server] initializes the server *)
val init_server : unit -> server_data

(* [check_username user_data username] returns true if the [username] is
   not in the [user_data] list, and false if it is. *)
val check_username : user_data -> string -> bool

(* [login server_data line] will check to see if a given username in
   [line] is found in the server_data and will respond with "false" if
   it is not or "UNAME_EXISTS" if it does.*)
val login : server_data -> string -> Reader.t -> Writer.t -> server_data

(* [signup server_data line] check if a given username in [line] is
   already used, if it is it will return "UNAME_EXISTS" or if not
   "true", then it will update the server_data to add a new user with
   username [line] and a blank password. *)
val signup :
  server_data -> string -> Reader.t -> Writer.t -> server_data

(* [parse server_data line r w]*)
val parse : server_data -> Reader.t -> Writer.t -> server_data
