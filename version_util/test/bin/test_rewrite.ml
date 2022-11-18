open Core

let () =
  let tmp_file = Filename_unix.temp_file "version_util" "" in
  Exn.protect
    ~finally:(fun () -> Sys_unix.remove tmp_file)
    ~f:(fun () ->
      let initial_contents_of_exe = In_channel.read_all "main.exe" in
      (* NO_VERSION_UTIL if BUILD_PROFILE=fast-build, the real one if =fast-exe *)
      let initial_version =
        Version_util.Expert.get_version_util ~contents_of_exe:initial_contents_of_exe
      in
      assert (Option.is_some initial_version);
      let new_exe_contents =
        Option.value
          ~default:initial_contents_of_exe
          (Version_util.Expert.insert_version_util
             ~contents_of_exe:initial_contents_of_exe
             [ { repo = "ssh://machine//path/to/repo"
               ; version = "0123456789abcdef0123456789abcdef01234567"
               }
             ])
      in
      [%test_result: string]
        (Version_util.Expert.get_version_util ~contents_of_exe:new_exe_contents
         |> Option.value_exn)
        ~expect:"ssh://machine//path/to/repo_0123456789ab";
      Out_channel.write_all tmp_file ~data:new_exe_contents;
      let perm = (Core_unix.lstat tmp_file).st_perm in
      Core_unix.chmod tmp_file ~perm:(perm lor 0o100);
      Sys_unix.command_exn (Sys.concat_quoted [ tmp_file; "version-util-must-be-set" ]);
      let exe_contents_after_removal =
        Version_util.Expert.remove_version_util ~contents_of_exe:new_exe_contents
        |> Option.value_exn
      in
      [%test_result: string]
        (Version_util.Expert.get_version_util ~contents_of_exe:exe_contents_after_removal
         |> Option.value_exn)
        ~expect:"NO_VERSION_UTIL")
;;

let () =
  let contents_of_exe = In_channel.read_all "main.exe" in
  let occurrences =
    Version_util.Expert.For_tests.count_pattern_occurrences ~contents_of_exe
  in
  [%test_result: int] ~expect:1 occurrences
;;
