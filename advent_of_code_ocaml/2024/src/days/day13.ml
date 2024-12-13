(* open Mdrp_lib *)

(* We have something like

   A = Xa : 94, Ya : 78
   B = Xb : 63, Yb : 28

   Xt = 8400, Yt = 5700

   We need to solve

   Ca.Xa + Cb.Xb = Xt
   Ca.Ya + Cb.Yb = Yt

   Ca.Xa.Yb + Cb.Xb.Yb = Xt.Yb
   Ca.Ya.Xb + Cb.Yb.Xb = Yt.Xb

   Ca.(Xa.Yb - Ya.Xb) = Xt.Yb - Yt.Xb
   Ca = (Xt.Yb - Yt.Xb)/(Xa.Yb - Ya.Xb)
   Cb = (Xt - Ca.Xa) / Xb *)

let solve xa ya xb yb xtot ytot =
  let ca = ((xtot * yb) - (ytot * xb)) / ((xa * yb) - (ya * xb)) in
  let cb = (xtot - (ca * xa)) / xb in
  if (ca * xa) + (cb * xb) = xtot && (ca * ya) + (cb * yb) = ytot then
    Some (ca, cb)
  else None

let common_part part file =
  let ci = open_in file in
  let re_button =
    Re.compile @@ Re.Perl.re {|Button (A|B): X\+(\d+), Y\+(\d+)|}
  in
  let re_prize = Re.compile @@ Re.Perl.re {|Prize: X=(\d+), Y=(\d+)|} in
  let rec loop tokens =
    let linea = Re.exec re_button (input_line ci) in
    let xa, ya =
      ( Re.Group.get linea 2 |> int_of_string,
        Re.Group.get linea 3 |> int_of_string )
    in
    let lineb = Re.exec re_button (input_line ci) in
    let xb, yb =
      ( Re.Group.get lineb 2 |> int_of_string,
        Re.Group.get lineb 3 |> int_of_string )
    in
    let prize = Re.exec re_prize (input_line ci) in
    let xtot, ytot =
      ( ((Re.Group.get prize 1 |> int_of_string)
        + if part = 2 then 10_000_000_000_000 else 0),
        (Re.Group.get prize 2 |> int_of_string)
        + if part = 2 then 10_000_000_000_000 else 0 )
    in
    let tokens =
      match solve xa ya xb yb xtot ytot with
      | Some (ca, cb) when part = 2 || (ca <= 100 && cb <= 100) ->
          (3 * ca) + cb + tokens
      | _ -> tokens
    in
    try
      (* Do this check after updating the number of tokens
         Otherwise, if the last group gives a result its number of tokens
         won't be added. No, I didn't lose 30 minutes on this *)
      ignore (input_line ci);
      loop tokens
    with _ -> tokens
  in
  loop 0

let run part file = common_part part file
