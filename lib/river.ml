open Eio.Std
open Wayland_protocols.River_window_management_v1_client
module Registry = Wayland.Registry

let hello = print_endline "hellope"

let protocols =
  [
    "river_window_manager_v1";
    "river_layer_shell_v1";
    "river_xkb_bindings_v1";
    "wp_single_pixel_buffer_manager_v1";
    "wl_compositor";
    "wp_viewporter";
  ]

type window = [ `V3 ] River_window_v1.t
type output = [ `V3 ] River_output_v1.t
type seat = [ `V3 ] River_seat_v1.t

type t = {
  mutable seats : seat array;
  mutable outputs : output array;
  mutable windows : window array;
}

(* Check if all protocols are supported by compositor *)
let check_registry (reg : Registry.t) =
  List.iter
    (fun x ->
      if List.is_empty @@ Registry.get reg x then begin
        Printf.eprintf "Compositor does not support %s, aborting..." x;
        exit 1
      end
      else ())
    protocols

let create_output (reg : Registry.t) =
  Registry.bind reg
  @@ object
       inherit [_] River_output_v1.v3
       val mutable is_removed : bool = false
       method bind_version = `V3

       method on_removed output =
         match is_removed with
         | true -> River_output_v1.destroy output
         | false -> is_removed <- true

       method on_wl_output _ ~name:_ = ()
       method on_position _ ~x:_ ~y:_ = ()
       method on_dimensions _ ~width:_ ~height:_ = ()
     end

let create_window (reg : Registry.t) =
  Registry.bind reg
  @@ object
       inherit [_] River_window_v1.v3
       method bind_version = `V3
       method on_closed _ = ()

       method on_dimensions_hint _ ~min_width:_ ~min_height:_ ~max_width:_
           ~max_height:_ =
         ()

       method on_dimensions _ ~width:_ ~height:_ = ()
       method on_app_id _ ~app_id:_ = ()
       method on_title _ ~title:_ = ()
       method on_parent _ ~parent:_ = ()
       method on_decoration_hint _ ~hint:_ = ()
       method on_pointer_move_requested _ ~seat:_ = ()
       method on_pointer_resize_requested _ ~seat:_ ~edges:_ = ()
       method on_show_window_menu_requested _ ~x:_ ~y:_ = ()
       method on_maximize_requested _ = ()
       method on_unmaximize_requested _ = ()
       method on_fullscreen_requested _ ~output:_ = ()
       method on_exit_fullscreen_requested _ = ()
       method on_minimize_requested _ = ()
       method on_unreliable_pid _ ~unreliable_pid:_ = ()
     end

let create_seat (reg : Registry.t) =
  Registry.bind reg
  @@ object
       inherit [_] River_seat_v1.v3
       method bind_version = `V3
       method on_removed _ = ()
       method on_wl_seat _ ~name:_ = ()
       method on_pointer_enter _ ~window:_ = ()
       method on_pointer_leave _ = ()
       method on_window_interaction _ ~window:_ = ()
       method on_shell_surface_interaction _ ~shell_surface:_ = ()
       method on_op_delta _ ~dx:_ ~dy:_ = ()
       method on_op_release _ = ()
       method on_pointer_position _ ~x:_ ~y:_ = ()
     end

let create_window_manager (reg : Registry.t) =
  Registry.bind reg
  @@ object
       inherit [_] River_window_manager_v1.v3

       method on_unavailable _ =
         print_endline "There is another window manager running! Exiting...";
         exit 1

       method on_finished _ = exit 0
       method on_seat _ _ = ()
       method on_manage_start _ = ()
       method on_render_start _ = ()
       method on_session_locked _ = ()
       method on_session_unlocked _ = ()
       method on_window _ _ = ()
       method on_output _ _ = ()
     end

let run =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level (Some Info));
  Printexc.record_backtrace true;
  Eio_main.run @@ fun env ->
  let net = env#net in
  Switch.run @@ fun sw ->
  let transport = Wayland.Unix_transport.connect ~sw ~net () in
  let display = Wayland.Client.connect ~sw transport in
  let reg = Registry.of_display display in
  check_registry reg;
  let _ = create_window_manager reg in
  while true do
    ()
  done

let river_of_ocaml = run
