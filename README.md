# Paint-GUI-Application

OCaml based paint application that extends the GUI toolkit and allows basic shapes/line drawing functionalities, color changes, changing line thickness

## Class Overview


### Paint 
The paint program uses a mutable record (called state) to store its state. This record stores the sequence of shapes that the user has drawn (state.shapes), the input mode of the paint program (state.mode), 
and the currently selected pen color (state.color). The GUI of the paint program starts with three components: the mode_toolbar, the color_toolbar, and the paint_canvas. These three widgets are laid out horizontally 
at the end of paint.ml (see below).

### Widget

A widget is a record with three fields which are all functions. These functions mean that a widget knows:

how to draw itself (repaint),
how to handle events (handle), and
how big it is (size).


### Paint Canvas: Event Handling
We handle clicks inside of the canvas using the paint_action function. This funcion is added as an event_listener to the canvas via its controller, paint_canvas_controller. This means that whenever an event (the ones defined in gctx.ml!) occurs in the canvas, the paint_action function is called.

To implement this, we define two drawing modes: LineStartMode and LineEndMode. A user's first click sets the start point of the line and switches the mode; the second click sets the end point and switches the mode back. This handling of MouseDown events is written in paint_action. We keep track of the first point by sticking it into the LineEndMode constructor; after we get the second click, we add the line to state.shapes, then we go back to LineStartMode. (Notice that we use state.color as the color component of the stored line.)

