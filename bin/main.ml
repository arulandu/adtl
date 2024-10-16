open Adtl

let usage = "Usage: tt [option] ... [file] ..."
let interactive_shell = ref true
let wrapper = ref (Some ["rlwrap"; "ledit"])

let help_text = "Toplevel commands:
Parameter <ident> : <expr>.    assume variable <ident> has type <expr>
Definition <indent> := <expr>. define <ident> to be <expr>
Check <expr>                   infer the type of expression <expr>
Eval <expr>.                   normalize expression <expr>
Context.                       print current contex    
Help.                          print this help
Quit.                          exit" ;;

(** A list of files to be loaded and run. *)
let files = ref []

let add_file interactive filename = (files := (filename, interactive) :: !files)

(** A list of command-line wrappers to look for. *)
let wrapper = ref (Some ["rlwrap"; "ledit"])

(** Command-line options *)
let options = Arg.align [
  ("--wrapper",
    Arg.String (fun str -> wrapper := Some [str]),
    "<program> Specify a command-line wrapper to be used (such as rlwrap or ledit)");
  ("--no-wrapper",
    Arg.Unit (fun () -> wrapper := None),
    " Do not use a command-line wrapper");
  ("-v",
    Arg.Unit (fun () ->
      print_endline ("tt " ^ Version.version ^ "(" ^ Sys.os_type ^ ")");
      exit 0),
    " Print version information and exit");
  ("-n",
    Arg.Clear interactive_shell,
    " Do not run the interactive toplevel");
  ("-l",
    Arg.String (fun str -> add_file true str),
    "<file> Load <file> into the initial environment");
  ("-lq",
    Arg.String (fun str -> add_file false str),
    "<file> Load <file> into the initial environment and quiet logs.");
]

(** Treat anonymous arguments as files to be run. *)
let anonymous str =
  add_file true str;
  interactive_shell := false

(** Parser wrapper *)
let parse parser lex =
  try
    parser Lexer.token lex
  with
  | Parser.Error ->
      Error.syntax ""
  | Failure "lexing: empty token" ->
      Error.syntax "unrecognised symbol."

let initial_ctx = []

(** [exec_cmd ctx d] executes toplevel directive [d] in global context [ctx]. It prints the
    result on standard output and return the new context. *)
let rec exec_cmd interactive ctx d =
  match d with
    | Ast.Eval e ->
      let t = Infer.infer_type ctx e in
      let e = Infer.normalize ctx e in
        if interactive then Format.printf "    = @[%t@]@\n    : @[%t@]@."
          (Print.expr e)
          (Print.expr t) ;
        ctx
    | Ast.Context ->
      List.iter
        (function
          | (x, (t, None)) -> Format.printf "@[%t : @[%t@]@]@." (Print.variable x) (Print.expr t)
          | (x, (t, Some e)) -> Format.printf "@[%t = %t@]@\n    : @[%t@]@."
            (Print.variable x) (Print.expr e) (Print.expr t))
        (List.rev ctx) ;
      ctx
    | Ast.Parameter (x, t) ->
      ignore (Infer.infer_universe ctx t) ;
      Ctx.extend x t ctx
    | Ast.Definition (x, e) ->
      let t = Infer.infer_type ctx e in
      if List.mem_assoc x ctx then begin
        let tx = Ctx.lookup_ty x ctx in
        if Infer.equal ctx t tx then
          Ctx.extend x tx ~value:e ctx
        else
          Error.typing "%t already exists and does not match type" (Print.variable x);
      end else
        Ctx.extend x t ~value:e ctx
    | Ast.Check e ->
      let t = Infer.infer_type ctx e in
        Format.printf "@[%t@]@\n    : @[%t@]@." (Print.expr e) (Print.expr t) ;
        ctx
    | Ast.Help ->
      print_endline help_text ; ctx
    | Ast.Quit -> exit 0

(** Load directives from the given file. *)
and use_file ctx (filename, interactive) =
  let cmds = Lexer.read_file (parse Parser.directives) filename in
    List.fold_left (exec_cmd interactive) ctx cmds

(** Interactive toplevel *)
let toplevel ctx =
  let eof = match Sys.os_type with
    | "Unix" | "Cygwin" -> "Ctrl-D"
    | "Win32" -> "Ctrl-Z"
    | _ -> "EOF"
  in
  print_endline ("tt " ^ Version.version);
  print_endline ("[Type " ^ eof ^ " to exit or \"Help.\" for help.]");
  try
    let ctx = ref ctx in
    while true do
      try
        let cmds = Lexer.read_toplevel (parse Parser.directives) () in
        ctx := List.fold_left (exec_cmd true) !ctx cmds
      with
        | Error.Error err -> Print.error err
        | Sys.Break -> prerr_endline "Interrupted."
    done
  with End_of_file -> ()

(** Main program *)
let main =
  Sys.catch_break true;
  (* Parse the arguments. *)
  Arg.parse options anonymous usage;
  (* Attempt to wrap yourself with a line-editing wrapper. *)
  if !interactive_shell then
    begin match !wrapper with
      | None -> ()
      | Some lst ->
          let n = Array.length Sys.argv + 2 in
          let args = Array.make n "" in
            Array.blit Sys.argv 0 args 1 (n - 2);
            args.(n - 1) <- "--no-wrapper";
            List.iter
              (fun wrapper ->
                 try
                  args.(0) <- wrapper;
                  let cmd = String.concat " " (Array.to_list args) in
                  let exit_status = Sys.command cmd in
                  if exit_status <> 0 then
                    Printf.printf "Failed to execute wrapper: %s\n" wrapper
                 with _ -> ())
              lst
    end;
  (* Files were listed in the wrong order, so we reverse them *)
  files := List.rev !files;
  try
    (* Run and load all the specified files. *)
    let ctx = List.fold_left use_file initial_ctx !files in
    if !interactive_shell then toplevel ctx
  with
    Error.Error err -> Print.error err; exit 1
