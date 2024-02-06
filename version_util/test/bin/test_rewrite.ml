open Core

let write_exe path ~contents_of_exe =
  Out_channel.write_all path ~data:contents_of_exe;
  let perm = (Core_unix.lstat path).st_perm in
  Core_unix.chmod path ~perm:(perm lor 0o100)
;;

let run_exe path arg = Sys_unix.command_exn (Sys.concat_quoted [ path; arg ])

let () =
  let test_exe = Filename_unix.temp_file "version_util" "" in
  Exn.protect
    ~finally:(fun () -> Sys_unix.remove test_exe)
    ~f:(fun () ->
      let initial_contents_of_exe = In_channel.read_all "main.exe" in
      (* NO_VERSION_UTIL if BUILD_PROFILE=fast-build, the real one if =fast-exe *)
      let initial_version =
        Version_util.Expert.get_version_util ~contents_of_exe:initial_contents_of_exe
      in
      assert (Option.is_some initial_version);
      let new_contents_of_exe =
        Version_util.Expert.replace_version_util
          ~contents_of_exe:initial_contents_of_exe
          (Some
             [ { repo = "ssh://machine//path/to/repo"
               ; version = "0123456789abcdef0123456789abcdef01234567"
               }
             ])
        |> Option.value_exn
      in
      [%test_result: string]
        (Version_util.Expert.get_version_util ~contents_of_exe:new_contents_of_exe
         |> Option.value_exn)
        ~expect:"ssh://machine//path/to/repo_0123456789ab";
      write_exe test_exe ~contents_of_exe:new_contents_of_exe;
      run_exe test_exe "version-util-must-be-set";
      let contents_of_exe_after_removal =
        Version_util.Expert.replace_version_util ~contents_of_exe:new_contents_of_exe None
        |> Option.value_exn
      in
      [%test_result: string]
        (Version_util.Expert.get_version_util
           ~contents_of_exe:contents_of_exe_after_removal
         |> Option.value_exn)
        ~expect:"NO_VERSION_UTIL")
;;

let () =
  let test_exe = Filename_unix.temp_file "build_info" "" in
  Exn.protect
    ~finally:(fun () -> Sys_unix.remove test_exe)
    ~f:(fun () ->
      let initial_contents_of_exe = In_channel.read_all "main.exe" in
      write_exe test_exe ~contents_of_exe:initial_contents_of_exe;
      run_exe test_exe "build-info-must-be-set";
      let new_contents_of_exe =
        Version_util.Expert.Experimental.remove_build_info
          ~contents_of_exe:initial_contents_of_exe
        |> Option.value_exn
      in
      write_exe test_exe ~contents_of_exe:new_contents_of_exe;
      run_exe test_exe "build-info-must-be-unset")
;;

let () =
  let contents_of_exe = In_channel.read_all "main.exe" in
  let occurrences =
    Version_util.Expert.For_tests.count_section_occurrences ~contents_of_exe
  in
  [%test_result: int] ~expect:2 occurrences
;;
