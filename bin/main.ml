open Unix

let version = "0.0.1"

type state = { cx : int ref; cy : int ref; term : terminal_io; buff : Buffer.t }

let state =
  { cx = ref 0; cy = ref 0; term = tcgetattr stdin; buff = Buffer.create 16 }

(** Set the terminal to non-canonical mode *)
let set_terminal_unbuffered () =
  Out_channel.set_buffered Out_channel.stdout false;

  let term = state.term in
  let new_term =
    {
      term with
      c_brkint = false;
      c_icrnl = false;
      c_inpck = false;
      c_istrip = false;
      c_ixon = false;
      c_opost = false;
      c_csize = 8;
      c_echo = false;
      c_icanon = false;
      c_isig = false;
      c_vmin = 0;
      c_vtime = 1;
    }
  in
  tcsetattr stdin TCSANOW new_term

let draw_rows () =
  let screen_rows =
    match Terminal_size.get_rows () with Some r -> r | None -> 80
  in
  let screen_cols =
    match Terminal_size.get_columns () with Some r -> r | None -> 80
  in
  for y = 0 to screen_rows do
    if y == screen_rows / 3 then (
      let welcome_str =
        Printf.sprintf "Flowrey's Own Editor -- version %s" version
      in
      let welcome_len =
        if String.length welcome_str > screen_cols then screen_cols
        else String.length welcome_str
      in

      let padding = ref ((screen_cols - welcome_len) / 2) in
      if !padding != 0 then (
        Buffer.add_string state.buff "~";
        padding := !padding - 1);

      for _ = 0 to !padding do
        Buffer.add_string state.buff " "
      done;

      Buffer.add_substring state.buff welcome_str 0 welcome_len)
    else Buffer.add_string state.buff "~";

    Buffer.add_string state.buff "\x1b[K";
    if y < screen_rows then Buffer.add_string state.buff "\r\n"
  done

let refresh_screen () =
  Buffer.add_string state.buff "\x1b[?25l";
  Buffer.add_string state.buff "\x1b[H";

  draw_rows ();
  let cursor =
    Printf.sprintf "\x1b[%d;%dH" (!(state.cy) + 5) (!(state.cx) + 5)
  in
  Buffer.add_string state.buff cursor;

  Buffer.add_string state.buff "\x1b[?25h";

  Buffer.output_buffer Out_channel.stdout state.buff;
  Buffer.clear state.buff

let moveCursor char =
  match char with
  | 'h' -> state.cx := !(state.cx) - 1
  | 'j' -> state.cy := !(state.cy) + 1
  | 'k' -> state.cy := !(state.cy) - 1
  | 'l' -> state.cx := !(state.cx) + 1
  | _ -> ()

let process_key char =
  match char with
  | Some 17 (* Ctrl+Q *) ->
      print_string "\x1b[2J";
      print_string "\x1b[H";
      tcsetattr stdin TCSANOW state.term;
      exit 0
  | Some ((104 | 106 | 107 | 108) as c) -> moveCursor (Char.chr c)
  | Some _ -> ()
  | None -> ()

let read_key_seq () =
  let rec next_char () =
    refresh_screen ();
    let char = In_channel.input_byte In_channel.stdin in
    match char with
    | Some c -> Seq.Cons (Some c, next_char)
    | None -> Seq.Cons (None, next_char)
  in
  next_char

let () =
  set_terminal_unbuffered ();

  let seq = read_key_seq () in
  Seq.iter process_key seq
