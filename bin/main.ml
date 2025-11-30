let rec read_user_input () =
  let user_input = In_channel.input_char In_channel.stdin in
  match user_input with
  | Some 'q' -> () (*quit*)
  | Some c ->
      output_char stdout c;
      output_char stdout '\r';
      output_char stdout '\n';
      read_user_input ()
  | None -> read_user_input ()

let () =
  (* Set `stdout` to unbuffered mode *)
  Out_channel.set_buffered Out_channel.stdout false;

  (* Set the terminal to raw mode *)
  let term = Unix.tcgetattr Unix.stdin in
  let new_term =
    {
      term with
      c_echo = false;
      c_icanon = false;
      c_isig = false;
      c_ixon = false;
      c_icrnl = false;
      c_opost = false;
      c_vmin = 0;
      c_vtime = 1;
    }
  in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW new_term;

  (* Start the main running loop *)
  read_user_input ();

  (* Restore the terminal to original mode *)
  Unix.tcsetattr Unix.stdin Unix.TCSANOW term
