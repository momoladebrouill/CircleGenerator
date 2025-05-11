open Raylib
open Maths

type direction = {
  vertical : float;
  horizontal : float;
}

type status = {
    camera : Camera3D.t;
    cubes : r3 list;
    t : float;
}

let sinus t cubes =
  let sinus cube =
    let x,y,_ = cube in
    (x, y,  1. +. (sin (t +. (norme_sq (x,y,0.0))/.100.0 )))
  in
  List.map sinus cubes

let generate_circle r =
    let (<<) = Int.shift_right in
    let rec loop (x,y,m) l = 
        if x <= y then
                let l' = (foi x,foi y,0.0)::l in
                let y',m',x' = 
                    if m > 0 then
                        (y-1, m + 8*(x-y) + 20, x+1)
                    else
                        (y, m + 8*x+ 12 ,x+1)
                in
                    loop (x',y',m') l' 
        else
            l
    in
    let huitieme = loop (0, r , 5 - 4*r) [] in
    let quart = huitieme @ (List.map (fun  (x,y,z) -> y,x,z) huitieme) in
    let demi = quart @ (List.map (fun (x,y,z) -> -.x,y,z) quart) in
     demi @ (List.map (fun (x,y,z) -> x,-.y,z) demi)

let rec generate_circle_full i = 
    if i = 0 then []
    else (generate_circle i) @ generate_circle_full (i-1)
    
let draw s = 
     clear_background Color.black;
     begin_mode_3d s.camera;
     draw_cube zero 1.0 1.0 1.0 Color.raywhite; 
     let l = List.length s.cubes in
     List.iteri (fun i pos -> if i <= iof (5.0*.s.t) then 
      draw_cube (r3_to_vec3 pos) 1.0 1.0 1.0 (color_from_hsv (foi (i*360/l)) 1. 1.);
      ()
      ) s.cubes;
     end_mode_3d ();
     begin_drawing ();
     draw_text "Space S X arrows" 10 10 20 Color.white;
     end_drawing ()

let update s =
    {
    camera = s.camera;
    cubes = sinus s.t s.cubes;
    t = s.t +. 0.1;
}

let rec loop s =
     if Raylib.window_should_close () then Raylib.close_window () else begin 
         draw s;
         Camera3D.set_projection s.camera CameraProjection.Perspective;
         update_camera (addr s.camera) CameraMode.Third_person;
         Vector3.set_y (Camera3D.target s.camera) 0.0;
         Vector3.set_x (Camera3D.target s.camera) 0.0 (*s.t |> iof |> List.nth s.cubes |> fst*);
         Vector3.set_z (Camera3D.target s.camera) 0.0 (*s.t |> iof |> List.nth s.cubes |> snd*);
         loop (update s) 
    end

let setup () =
  Raylib.init_window w h "vichy";
  Raylib.set_target_fps 60;
  if is_window_ready () then
      let cam  = Camera3D.create zero zero (Vector3.create 0. 1. 0.) 45. CameraProjection.Perspective in
      Camera3D.set_position cam (Vector3.create 0.0 20.0 40.0);

  {
      camera = cam;
      t = 0.0;
      cubes = generate_circle_full 30;
   }
   else raise (Invalid_argument "window not ready")

let () = () |> setup |> loop; 
