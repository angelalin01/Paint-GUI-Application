module Graphics = G

(** The "Graphics Context" component of the GUI library. *)

(** A graphics context represents a portion of the window to which
    widgets will be drawn.

    The drawing primitives in this module are all relative to the
    graphics context. This means that when a widget needs to draw on the
    screen, it need not know its absolute position. The graphics context
    is responsible for translating the relative positions passed into the
    drawing routines into absolute positions on the screen.

    The graphics context also includes other information for basic
    drawing (such as the current pen color.) *)


(****************)
(** {1 Colors } *)
(****************)

(** A type for colors *)
type color = {r:int; g:int; b:int}

let black   : color = {r=0;  g=0;  b=0}
let white   : color = {r=255;g=255;b=255}
let red     : color = {r=255;g=0;  b=0}
let green   : color = {r=0;  g=255;b=0}
let blue    : color = {r=0;  g=0;  b=255}
let yellow  : color = {r=255;g=255;b=0}
let cyan    : color = {r=0;  g=255;b=255}
let magenta : color = {r=255;g=0;  b=255}


(*******************************)
(** Basic Gctx operations      *)
(*******************************)

(** The main type of graphics contexts. Note that none of the components
    are mutable. *)

type gctx = {
  x: int;         (** offset from (0,0) in local coordinates *)
  y: int;
  color: color;   (** the current pen color *)
  thickness: int;
  }


(* Internal helper to set the text size *)
let set_text_size (text_size: int) (font: string) : unit =
  if Graphics.js_mode then Graphics.set_font font;
  Graphics.set_text_size text_size

let clear_graph () =
  Graphics.clear_graph ();
  set_text_size 15 (if Graphics.js_mode then "sans-serif" else "")

(* has the graphics window been opened already? *)
let graphics_opened = { contents = false; }
(** Open the graphics window, but only do it once *)
let open_graphics () =
  if not graphics_opened.contents then
    begin
      graphics_opened.contents <- true;
      Graphics.open_graph
        (if Graphics.js_mode then "width=640,height=480" else "");
      Graphics.resize_window 740 580;
      if not Graphics.js_mode then Graphics.auto_synchronize false;
      clear_graph ()
    end


(** The top-level graphics context. *)
let top_level : gctx =
  { x = 0;
    y = 0;
    color = black;
    thickness = 1;
     }

(** Widgets can translate (move) a graphics context to obtain an
    appropriate graphics context for their children. *)

(** Shift the gctx by (dx,dy) *)
let translate (g: gctx) ((dx, dy): int * int) : gctx =
  { g with x = g.x + dx; y = g.y + dy }

(** Produce a new Gctx.t with a different pen color *)
let with_color (g: gctx) (c: color) : gctx =
  { g with color = c }

let with_thickness (g: gctx) (t: int) : gctx =
  { g with thickness = t}

(** Set the OCaml graphics library's internal state so that it
    agrees with the Gctx settings.

    Initially, this just sets the current pen color.
*)

let set_graphics_state (gc: gctx) : unit =
  let c = gc.color in
  let thickness = gc.thickness in
  Graphics.set_color (Graphics.rgb c.r c.g c.b);
  Graphics.set_line_width (thickness)


(************************************)
(*    Coordinate Transformations    *)
(************************************)

(* The default width and height of the graphics window that OCaml opens.   *)

let graphics_size_x () =
  if graphics_opened.contents then Graphics.size_x () else 640
let graphics_size_y () =
  if graphics_opened.contents then Graphics.size_y () else 480

(* A main purpose of the graphics context is to provide mapping between    *)
(* widget-local coordinates and the ocaml coordinates of the graphics      *)
(* library. Part of that translation comes from the offset stored in the   *)
(* graphics context itself. The translation needs to know where the widget *)
(* is on the screen. The other part of the translation is the y axis flip. *)
(* The OCaml library puts (0,0) at the bottom left corner of the window.   *)
(* We'd like our GUI library to put (0,0) at the top left corner and       *)
(* increase the y-coordinate as we go *down* the screen.                   *)

(** A widget-relative position *)
type position = int * int

(* ALWAYS call these functions before passing widget-local points to the   *)
(* OCaml native Graphics module or vice-versa.                             *)

(** Convert widget-local coordinates (x,y) to OCaml graphics
    coordinates, relative to the graphics context.                         *)
let ocaml_coords (g: gctx) ((x, y): position) : (int * int) =
  (g.x + x, graphics_size_y () - 1 - (g.y + y))

(** Convert OCaml Graphics coordinates (x,y) to widget-local graphics
    coordinates, relative to the graphics context                          *)
let local_coords (g: gctx) ((x, y): int * int) : position =
  (x - g.x, (graphics_size_y () - 1 - y) - g.y)



(*****************)
(** {1 Drawing } *)
(*****************)

(** A width and height, paired together. *)
type dimension = int * int

(* Each of these functions takes inputs in widget-local coordinates,       *)
(* converts them to OCaml coordinates, and then draws the appropriate      *)
(* shape.                                                                  *)

(** Draw a line between the two specified positions *)
let draw_line (g: gctx) (p1: position) (p2: position) : unit =
 set_graphics_state g;
  let (x1, y1) = ocaml_coords g p1 in
  let (x2, y2) = ocaml_coords g p2 in
  Graphics.moveto x1 y1;
  Graphics.lineto x2 y2
  

(** Display text at the given position *)
let draw_string (g: gctx) (p: position) (s: string) : unit =
  set_graphics_state g;
  let (_, height) = Graphics.text_size s in
  let (x, y) = ocaml_coords g p in
  (* Web browser font rendering bounding box adjusment *)
  let fudge = if Graphics.js_mode then 3 else 0 in 
  (* subtract: working with Ocaml coordinates *)
  Graphics.moveto x (y - height + fudge);
  Graphics.draw_string s
  

(** Display a rectangle with lower-left corner at position
    with the specified dimension. *)
                                             
let draw_rect (g: gctx) (p1: position) ((w, h): dimension): unit =
  set_graphics_state g;
  let (x1, y1) = ocaml_coords g p1 in
  Graphics.draw_rect x1 y1 w h 
 
  

(** Display a filled rectangle with lower-left corner at positions
    with the specified dimension. *)
let fill_rect (g: gctx) (p1: position) ((w, h): dimension): unit =
  set_graphics_state g;
  let (x, y) = ocaml_coords g p1 in
  Graphics.fill_rect x y w h
  

(** Draw an ellipse at the given position with the given radii *)
(* TODO: you will need to make this function actually draw an
   ellipse for Task 0.  *)
let draw_ellipse (g: gctx) (p: position) (rx: int) (ry: int): unit =
 set_graphics_state g;
  let (x1, y1) = ocaml_coords g p in
  Graphics.draw_ellipse x1 y1 rx ry
  

(** Calculates the size of a text when rendered. *)
let text_size (text: string) : dimension =
  (* Make sure that the graphics window has been opened. All of the other 
     functions require a graphics context, which is difficult to get without
     opening the graphics window first. But this one does not, so is a 
     source of subtle bugs. *)
 open_graphics ();
  let (w,h) = Graphics.text_size text in
  (w+1, h)  (* Web browser font widths seem to be smaller than desirable *)


let draw_point (g: gctx) (p: position) : unit = 
  set_graphics_state g; 
  let (x, y) = ocaml_coords g p in
  Graphics.plot x y
  
  
 let draw_points (g: gctx) (plist: position list) : unit = 
  set_graphics_state g; 
  List.iter (draw_point g) (plist);
  
 

(************************)
(** {1 Event Handling } *)
(************************)

(* This part of the module adapts OCaml's native event handling to     *)
(* something that more closely resembles that found in Java.           *)

(** Types of events that could occur *)
type event_type =
  | KeyPress of char    (* Key pressed on the keyboard.      *)
  | MouseDown           (* Mouse button pressed.             *)
  | MouseUp             (* Mouse button released.            *)
  | MouseMove           (* Mouse moved with the button up.   *)
  | MouseDrag           (* Mouse moved with the button down. *)


let string_of_event_type (et : event_type) : string =
  begin match et with
    | KeyPress k -> "KeyPress at " ^ (String.make 1 k)
    | MouseDrag  -> "MouseDrag"
    | MouseMove  -> "MouseMove"
    | MouseUp    -> "MouseUp"
    | MouseDown  -> "MouseDown"
  end


(** An event records its type and the widget-local position of
    the mouse when the event occurred. *)
type event = event_type * position

(** Accessor for the type of an event. *)
let event_type (e: event) : event_type =
  fst e


(** Accessor for the widget local position of an event. *)
let event_pos (e: event) (g : gctx) : position =
  local_coords g (snd e)

(** Convert an event to a string *)
let string_of_event ((ty, (x, y)): event) : string =
  (string_of_event_type ty) ^ " at " ^ (string_of_int x) ^ ","
  ^ (string_of_int y)

(** Make a dummy event for testing. *)
let make_test_event (et : event_type) ((x, y) : position) =
  (et, (x, graphics_size_y () - y))
