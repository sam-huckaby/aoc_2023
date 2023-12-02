(*
Plan:
        Iterate over each string in the list of calibration values
        I want to only iterate over as many elements as possible, preferrably O/2 complexity
        I could loop from 0 -> length_of_string / 2
        So for each loop:
                Grab the first and last character
                If either character is a number, return a tuple containing it e.g. (str.[i], (get_calibration str))
 *)

let explode str = List.init(String.length str) (String.get str);;

let m'digit c =
        match c with
        | '0' .. '9' -> true
        | _ -> false

(*let m'nums = List.map (Printf.sprintf "%c") (List.filter m'digit (explode "5five8twoonesf2"))*)

let head'n'tail m'list =
        let rec m'tail (sub'list: string list): string =
                match sub'list with
                | [] -> ""
                | [ h ] -> h
                | _ :: t -> (m'tail t)
        in
        match m'list with
        | [] -> "0"
        | [ h ] -> h ^ h
        | h :: t -> h ^ (m'tail t)

let () = 
        let counter = ref 0 in
        let input_channel = open_in "./input_day_1.txt" in
        try
                while true do
                        let line = input_line input_channel in
                        (* Process the line *)
                        counter := !counter + int_of_string (head'n'tail (List.map (Printf.sprintf "%c")  (List.filter m'digit (explode line))))
                done
        with End_of_file ->
                print_int !counter;
                close_in input_channel
