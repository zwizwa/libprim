#use "topfind";;
#require "FrontC";;

open Frontc;;

Frontc.parse_file 
  (* "/home/tom/libprim/ex/ex.c" *)
  "test.c"
  stderr ;;
