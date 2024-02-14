open Nottui
module W = Nottui_widgets
open Lwd_infix

type patch = unit

let pure_str s = Lwd.pure (W.string s)

let string_of_counter c =
  let$ c = c in
  W.string (string_of_int c)

let quit = Lwd.var false
let counter = Lwd.var 0
let counter_d = Lwd.get counter

let view =
  W.scrollbox
    (W.vbox
       [
         pure_str "Hello world!";
         string_of_counter counter_d;
         Lwd.pure
         @@ Ui.keyboard_area
              (function
                | `ASCII 'q', [] ->
                    Lwd.set quit true;
                    `Handled
                | `ASCII 'a', [] ->
                    Lwd.set counter (Lwd.peek counter + 1);
                    `Handled
                | `ASCII 'b', [] ->
                    Lwd.set counter (Lwd.peek counter - 1);
                    `Handled
                | _ -> `Unhandled)
              (W.string "Type 'q' to quit.");
       ])

let start () = Ui_loop.run ~quit ~tick_period:0.2 view
