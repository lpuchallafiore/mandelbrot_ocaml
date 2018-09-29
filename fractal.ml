open! Base
open! Js_of_ocaml
open Caml.Complex

module Html = Dom_html
let document = Html.window##.document
let jsopt_value_exn x = Js.Opt.get x (fun () -> assert false)

module Color = struct
  type t =
    { red   : float
    ; green : float
    ; blue  : float
    }

  let of_float ~r ~g ~b =
    { red = r; green = g; blue = b }

  let to_fillstyle t =
    Js.string (Caml.Format.sprintf "rgb(%f, %f, %f)"
      (255. *. t.red) (255. *. t.green) (255. *. t.blue))

  (* From http://www.iquilezles.org/www/articles/palettes/palettes.htm *)
  let cos_palette t =
    let a = of_float ~r:0.5 ~g:0.5 ~b:0.5 in
    let b = of_float ~r:0.5 ~g:0.5 ~b:0.5 in
    let c = of_float ~r:1.0 ~g:1.0 ~b:1.0 in
    let d = of_float ~r:0.0 ~g:0.1 ~b:0.2 in
    let pi2 = 6.28318 in
    { red   = a.red   +. b.red   *. Float.cos(pi2 *. (c.red   *. t +. d.red))
    ; green = a.green +. b.green *. Float.cos(pi2 *. (c.green *. t +. d.green))
    ; blue  = a.blue  +. b.blue  *. Float.cos(pi2 *. (c.blue  *. t +. d.blue))
    }
end

module Screen : sig
  type t
  val width  : t -> int
  val height : t -> int
  val create : (int * int) -> string -> t
  val pset   : t -> x:int -> y:int -> color:Color.t -> unit
end = struct
  type t =
    { context : Html.canvasRenderingContext2D Js.t
    ; width   : int
    ; height  : int
    }

  let width t = t.width
  let height t = t.height

  let create_canvas (w, h) =
    let canvas = Html.createCanvas document in
    canvas##.width := w;
    canvas##.height := h;
    canvas
  ;;

  let create ((w, h) as dim) div_name =
    let canvas_div =
      jsopt_value_exn (document##getElementById (Js.string div_name)) in
    let canvas = create_canvas dim in
    Dom.appendChild canvas_div canvas;
    let context = canvas##getContext (Html._2d_) in
    { context; width = w; height = h; }
  ;;

  let pset t ~x ~y ~color =
    t.context##.fillStyle := Color.to_fillstyle color;
    t.context##fillRect (Float.of_int x) (Float.of_int y) 1. 1.
end

let draw_mandelbrot (screen : Screen.t) =
  let rec mandelbrot i c z =
    if i=63 || Float.(>) (norm2 z) 4.
    then i
    else mandelbrot (i+1) c (add (mul z z) c)
  in
  let w = Screen.width screen in
  let h = Screen.height screen in
  for a = 0 to w - 1 do
    for b = 0 to h - 1 do
      let x = 2.5 *. Float.of_int a /. Float.of_int w -. 2. in
      let y = 2. *. Float.of_int b /. Float.of_int h -. 1. in
      let i = mandelbrot 0 { re=x; im=y } zero in
      let c = Color.cos_palette (0.2 +. 0.5 *. (Float.of_int i /. 63.)) in
      Screen.pset screen ~x:a ~y:b ~color:c
    done;
  done
;;

let main dim =
  Html.window##.onload := Html.handler (fun _ ->
    let screen = Screen.create dim "board" in
    let () = draw_mandelbrot screen in
    Js._false)
;;

let () = main (640, 480)
