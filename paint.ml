 (** Paint application *)

;; open Gctx
;; open Widget

(******************************************)
(**    SHAPES, MODES and PROGRAM STATE    *)
(******************************************)

(** The paint program uses the mutable record (called [state] below)
    to store its state.  *)

(** A location in the paint_canvas widget *)
type point = position (* from Gctx *)

(** The shapes that are visible in the paint canvas -- these make up the
    picture that the user has drawn, as well as any other "visible" elements
    that must show up in the canvas area (e.g. a "selection rectangle"). At
    the start of the homework, the only available shape is a line.  *)
(* TODO: You will modify this definition in Tasks 2, 4, 5 and maybe 6. *)
type shape = 
  | Line of int * color * point * point
  | Points of int * Gctx.color * point list
  | Ellipse of int * Gctx.color * point * point

(** These are the possible interaction modes that the paint program might be
    in. Some interactions require two modes. For example, the GUI might
    recognize the first mouse click as starting a line and a second mouse
    click as finishing the line.

    To start out, there are only two modes:

    - LineStartMode means the paint program is waiting for the user to make
    the first click to start a line.

    - LineEndMode means that the paint program is waiting for the user's
    second click. The point associated with this mode stores the location of
    the user's first mouse click.  *)
(* TODO: You will need to modify this type in Tasks 2, 4, and maybe 6. *)
type mode = 
  |PointMode
  |LineStartMode
  |LineEndMode of point
  |EllipseStartMode
  |EllipseEndMode of point 

(** The state of the paint program. *)
type state = {
  (** The sequence of all shapes drawn by the user, in order from
      least recent (the head) to most recent (the tail). *)
  shapes : shape Deque.deque;

  (** The input mode the Paint program is in. *)
  mutable mode : mode;

  (** The currently selected pen color. *)
  mutable color : color;
  
  mutable preview: shape option;
  
  mutable thick: bool;

  (* TODO: You will need to add new state for Tasks 2, 3, 5, and *)
  (* possibly 6 *) 
}

(** Initial values of the program state. *)
let paint : state = {
  shapes = Deque.create ();
  mode = LineStartMode;
  color = black;
  preview = None; 
  thick = false;
  (* TODO: You will need to add new state for Tasks 3, 5, and maybe 6 *)
  
}



(** This function creates a graphics context with the appropriate
    pen color.
*)
(* TODO: Your will need to modify this function in Task 5 *)
let with_params (g: gctx) (c: color)(t: int) : gctx =
  let g = with_color g c in
  let g2 = with_thickness g t in 
  g2

(*********************************)
(** PAINT CANVAS REPAINTING      *)
(*********************************)
(** The paint_canvas repaint function.

    This function iterates through all the drawn shapes (in order of least
    recent to most recent so that they are layered on top of one another
    correctly) and uses the Gctx.draw_xyz functions to display them on the
    canvas.  *)

(* TODO: You will need to modify this repaint function in Tasks 2, 3, 4,   *)
(* and possibly 6. For example, if the user is performing some operation   *)
(* that provides "preview" (see Task 2) the repaint function must also     *)
(* show the preview.                                                       *)
let repaint (g: gctx) : unit =
  let draw_shape (s: shape) : unit =
    begin match s with
      | Line (x,c,p1, p2) -> draw_line (with_params g c x) p1 p2;
      | Points (x,c, ps) -> draw_points (with_params g c x) ps; 
                           paint.color <- c; 
      | Ellipse (x,c, p1, p2) -> let (x1, y1) = p1 in
                               let (x2, y2) = p2 in 
                               let center = ((x1 + x2)/2, (y1 + y2)/2) in 
                               let rx = abs((x1 - x2)/2) in 
                               let ry = abs((y1 - y2)/2) in 
                               draw_ellipse (with_params g c x) center rx ry;
      
    end in
  Deque.iterate draw_shape paint.shapes;
 
   begin match paint.preview with 
   |None -> ()
   |Some preview -> draw_shape preview 
   end 

(** Create the actual paint_canvas widget and its associated
    notifier_controller . *)
let ((paint_canvas : widget), (paint_canvas_controller : notifier_controller)) =
  canvas (400, 250) repaint 

(************************************)
(**  PAINT CANVAS EVENT HANDLER     *)
(************************************)

(** The paint_action function processes all events that occur 
    in the canvas region. *)
(* TODO: Tasks 2, 3, 4, 5, and 6 involve changes to paint_action. *)
let paint_action (gc:gctx) (event:event) : unit =
  let p  = event_pos event gc in  (* mouse position *)
  let thick = if paint.thick then 7 else 1 in
  begin match (event_type event) with
    | MouseDown ->
      (* This case occurs when the mouse has been clicked in the canvas, but *)
      (* before the button has been released. How we process the event       *)
      (* depends on the current mode of the paint canvas.                    *)
      begin match paint.mode with 
         | LineStartMode ->
           (* The paint_canvas was waiting for the first click of a line,   *)
           (* so change it to LineEndMode, recording the starting point of  *)
           (* the line.                                                     *)
           paint.mode <- LineEndMode p; 
         |LineEndMode point -> () 
         |PointMode -> paint.preview <- 
         Some(Points(thick, paint.color, [p]))
         | EllipseStartMode -> paint.mode <- EllipseEndMode p 
         | EllipseEndMode point-> ()
       end
    | MouseDrag ->
        begin match paint.mode with 
      (* In this case, the mouse has been clicked, and it's being dragged    *)
      (* with the button down. Initially there is nothing to do, but you'll  *)
      (* need to update this part for Task 2, 3, 4 and maybe 6.              *)
      | LineStartMode -> paint.mode <- LineEndMode p
      | LineEndMode p1 -> paint.preview <- 
      Some (Line (thick,paint.color, p1, p))
      | PointMode -> let points = 
                      begin match paint.preview with 
                      |Some (Points(_,_, ps)) -> ps
                      | _ -> []
                      end in
                      paint.preview <- Some(Points(thick, paint.color, points))
       
      | EllipseStartMode -> paint.mode <- EllipseEndMode p
      | EllipseEndMode point -> paint.preview <- 
      Some (Ellipse (thick, paint.color, p, point))
        end
    | MouseUp -> 
      begin match paint.mode with 
      | LineStartMode -> paint.mode <- LineEndMode p
      | LineEndMode p1 -> Deque.insert_tail (Line (thick, paint.color, p1, p)) paint.shapes;
                      paint.mode <- LineStartMode; 
                      paint.preview <- None;
      | PointMode -> begin match paint.preview with 
                     |None -> ()
                     |Some shape -> Deque.insert_tail shape paint.shapes; 
                      paint.preview <- None
                     end
     | EllipseStartMode -> paint.mode <- EllipseEndMode p  
     | EllipseEndMode point -> Deque.insert_tail 
                     (Ellipse (thick, paint.color, point, p)) paint.shapes;
                      paint.mode <- EllipseStartMode; 
                      paint.preview <- None;
                   
      (* In this case there was a mouse button release event. TODO: Tasks 2, *)
      (* 3, 4, and possibly 6 need to do something different here.           *)
      end
    
    | _ -> ()
    (* This catches the MouseMove event (where the user moved the mouse over *) 
    (* the canvas without pushing any buttons) and the KeyPress event (where *)
    (* the user typed a key when the mouse was over the canvas).             *)
  end

(** Add the paint_action function as a listener to the paint_canvas *)
;; paint_canvas_controller.add_event_listener paint_action

(**************************************)
(** TOOLBARS AND PAINT PROGRAM LAYOUT *)
(**************************************)

(**
   This part of the program creates the other widgets for the
   paint program -- the buttons, color selectors, etc., and
   lays them out in the top - level window.

*)
(* TODO: Tasks 1, 2, 4, 5, and 6 involving adding new buttons or changing  *)
(* the layout of the Paint GUI. Initially the layout is very ugly because  *)
(* we use only the hpair widget demonstrated in Lecture. Task 1 is to make *)
(* improvements to make the layout more appealing. You may choose to       *)
(* arrange the buttons and other GUI elements of the paint program however *)
(* you like (so long as it is easily apparent how to use the interface ).  *)
(* The sample screen shot of our solution provides one possible design.    *)
(* Also, feel free to improve the visual components of the GUI, for        *)
(* example, our solution puts borders around the buttons and uses a custom *)
(* "color button" that changes its appearance based on whether or not the  *)
(* color is currently selected.                                            *)

(** Create the Undo button *)
let (w_undo, lc_undo, nc_undo) = button "Undo"

(**
   This function runs when the Undo button is clicked.
   It simply removes the last shape from the shapes deque.
*)
(* TODO: You need to modify this in Task 3 and 4. *)
let undo () : unit =
  if Deque.is_empty paint.shapes then () else
    ignore (Deque.remove_tail paint.shapes); paint.preview <- None; 
      begin match paint.mode with 
      |LineStartMode -> ()
      |LineEndMode p1 -> paint.mode <- LineStartMode;
      |PointMode -> ()
      |EllipseStartMode -> ()
      |EllipseEndMode p1 -> paint.mode <- EllipseStartMode;
      end

;; nc_undo.add_event_listener (mouseclick_listener undo)

(** The Quit button, with associated functionality. *)
let w_quit, lc_quit, nc_quit = button "Quit"

;; nc_quit.add_event_listener (mouseclick_listener (fun () -> exit 0))


(** Point button *)
let w_point, lc_point, nc_point = button "Point"

let pointButton () : unit = 
paint.mode <- PointMode

;; nc_point.add_event_listener (mouseclick_listener pointButton)

(** Line button *)
let w_line, lc_line, nc_line = button "Line"

let lineButton () : unit = 
paint.mode <- LineStartMode

;; nc_line.add_event_listener (mouseclick_listener lineButton)

(** Ellipse button *)

let w_ellipse, lc_ellipse, nc_ellipse = button "Ellipse"

let ellipseButton () : unit = 
paint.mode <- EllipseStartMode

;; nc_ellipse.add_event_listener (mouseclick_listener ellipseButton)

(** A spacer widget *)
let spacer : widget = space (10,10)

(** A color slider widget *)

let (blue_widget, blue_valueController) =
Widget.slider Gctx.blue 0 "Blue Slider" 

let (green_widget, green_valueController) =
Widget.slider Gctx.green 0 "Green Slider"

let (red_widget, red_valueController) = 
Widget.slider Gctx.red 0 "Red Slider"

  
  ;; blue_valueController.add_change_listener 
  (fun x -> paint.color <- {r = paint.color.r; 
  g = paint.color.g; b = x})

 
  ;; green_valueController.add_change_listener 
  (fun x -> paint.color <- {r = paint.color.r; 
  g = x; b = paint.color.b})
  
  ;; red_valueController.add_change_listener 
  (fun x -> paint.color <- {r = x; 
  g = paint.color.g; b = paint.color.b})
  
  let (rslider_label_widget, slider_label_controller) = label "Red Slider"
  let (bslider_label_widget, slider_label_controller) = label "Blue Slider"
  let (gslider_label_widget, slider_label_controller) = label "Green Slider"

(** A thickness checkbox widget *)
let (checkbox_widget, checkbox_valueController) = checkbox false "Thick Line"

let checkBoxState : (bool -> unit) = 
(fun b -> if b then paint.thick <- true else paint.thick <- false)

;; checkbox_valueController.add_change_listener(checkBoxState)

(** The mode toolbar, initially containing just the Undo and Quit buttons. *)
(*  TODO: you will need to add more buttons to the toolbar in *)
(*  Tasks 3, 5, and possibly 6. *)
let mode_toolbar : widget = hpair w_quit (hpair spacer (hpair w_undo 
(hpair spacer (hpair w_point (hpair spacer (hpair w_line 
(hpair spacer (hpair w_ellipse (hpair spacer (checkbox_widget))))))))))

let line_toolbar : widget = w_point
let point_toolbar :widget = w_line
let ellipse_toolbar : widget = w_ellipse 

(* The color selection toolbar. *)
(* This toolbar contains an indicator for the currently selected color 
   and some buttons for changing it. Both the indicator and the buttons 
   are small square widgets built from this higher-order function. *)
(** Create a widget that displays itself as colored square with the given 
    width and color specified by the [get_color] function. *)
let colored_square (width:int) (get_color:unit -> color)
  : widget * notifier_controller =
  let repaint_square (gc:gctx) =
    let c = get_color () in
    fill_rect (with_color gc c) (0, width-1) (width-1, width-1) in   
  canvas (width,width) repaint_square

(** The color_indicator repaints itself with the currently selected 
    color of the paint application. *)
let color_indicator =
  let indicator,_ = colored_square 24 (fun () -> paint.color) in
  let lab, _ = label "Current Color" in
  border (hpair lab indicator)

(** color_buttons repaint themselves with whatever color they were created 
    with. They are also installed with a mouseclick listener
    that changes the selected color of the paint app to their color. *)  
let color_button (c: color) : widget =
  let w,nc = colored_square 10 (fun () -> c) in
  nc.add_event_listener (mouseclick_listener (fun () ->
      paint.color <- c ));
  w
(** The color selection toolbar. Contains the color indicator and 
    buttons for several different colors. *)
   let color_toolbar : widget =
   hpair (hpair color_indicator spacer)
   (hpair (hpair (color_button black) spacer)
   (hpair (hpair (color_button white) spacer)
   (hpair (hpair (color_button red) spacer)
   (hpair (hpair (color_button green) spacer)
   (hpair (hpair (color_button blue) spacer)
   (hpair (hpair (color_button yellow) spacer)
   (hpair (hpair (color_button cyan) spacer)
   (color_button magenta))))))))

(** The top-level paint program widget: a combination of the
    mode_toolbar, the color_toolbar and the paint_canvas widgets.
*)
(* TODO: Task 1 (and others) modify the layout to add new buttons and make *)
(* the layout more aesthetically appealing.                                *)
let paint_widget : widget =
   vlist [color_toolbar; spacer; mode_toolbar; paint_canvas; spacer; 
   border blue_widget; 
   bslider_label_widget; spacer; 
   border red_widget; rslider_label_widget; spacer; 
   border green_widget; gslider_label_widget]
   
  
(**************************************)
(** Start the application             *)
(**************************************)

(** Run the event loop to process user events. *)
;; Eventloop.run paint_widget
