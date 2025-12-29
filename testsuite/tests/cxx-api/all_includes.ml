(* TEST
 has-cxx;
 hassysthreads;
 readonly_files = "deps.sh all-includes.h.sh cxx-sarif.sh stubs.cpp";
 include runtime_events;
 include systhreads;
 script = "sh ${test_source_directory}/deps.sh";
 script;
 {
   setup-ocamlopt.byte-build-env;
   script = "sh all-includes.h.sh";
   script;
   script = "sh cxx-sarif.sh";
   script;
   all_modules = "stubs.${objext} all_includes.ml";
   ocamlopt.byte;
   output = "${test_build_directory}/program-output";
   stdout = "${output}";
   run;
   check-program-output;
 }
 {
   setup-ocamlc.byte-build-env;
   script = "sh all-includes.h.sh";
   script;
   script = "sh cxx-sarif.sh";
   script;
   all_modules = "stubs.${objext} all_includes.ml";
   flags = "-output-complete-exe  -cclib -lunixbyt -cclib -lthreads -cclib -lcamlruntime_eventsbyt";
   ocamlc.byte;
   output = "${test_build_directory}/program-output";
   stdout = "${output}";
   run;
   check-program-output;
 }
*)

external test_cxx : unit -> string = "test_cxx"

let () = print_string (test_cxx ())
