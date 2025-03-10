module Distro = Dockerfile_opam.Distro
module Windows = Dockerfile_opam.Windows

module Switch_map = Map.Make(Ocaml_version)
module Windows_map = Map.Make(Distro)

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day Conf.days_between_rebuilds) ()
let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) ()

let win_ver ocluster =
  Conf.windows_distros
  |> List.fold_left (fun m (distro, product, pool) ->
    let w = Win_ver.get ~schedule:daily ocluster product pool in
    Windows_map.add distro w m) Windows_map.empty

let git_repositories () =
  Git_repositories.get ~schedule:weekly

(* [aliases_of d] gives other tags which should point to [d].
   e.g. just after the Ubuntu 20.04 release, [aliases_of ubuntu-20.04 = [ ubuntu; ubuntu-lts ]] *)
let aliases_of =
  let latest = Distro.distros |> List.map (fun d -> (Distro.resolve_alias d : Distro.distro :> Distro.t), d) in
  fun d -> List.filter_map (fun (d2, alias) -> if d = d2 && d <> alias then Some alias else None) latest

let master_distro = Distro.((resolve_alias master_distro : distro :> t))

type 'a run = ('a, unit, string, Dockerfile.t) format4 -> 'a

(* TODO: This opam repository overlay contains patches to OCaml which allows version < 4.08 to build on GCC14
 * See https://github.com/ocurrent/docker-base-images/issues/279 *)
let maybe_add_overlay (run : 'a run) distro switch =
  let gcc14_minimum = Ocaml_version.Releases.v4_08 in
  match Ocaml_version.compare switch gcc14_minimum < 0, distro with
  | true, `Alpine `V3_21
  | true, `Archlinux `Latest
  | true, `Debian `Testing
  | true, `Debian `Unstable
  | true, `Fedora `V40
  | true, `Fedora `V41
  | true, `Ubuntu `V24_10
  | true, `OpenSUSE `Tumbleweed ->
    run "opam repo add ocaml-patches-overlay git+https://github.com/ocurrent/opam-repository#patches --set-default"
  | _, _ ->
    Dockerfile.empty

let maybe_add_beta (run : 'a run) switch =
  let switch = Ocaml_version.without_variant switch in
  if Ocaml_version.Releases.is_dev switch ||
      List.mem switch Ocaml_version.Releases.unreleased_betas then
    run "opam repo add beta git+https://github.com/ocaml/ocaml-beta-repository --set-default"
  else
    Dockerfile.empty

let maybe_add_multicore (run : 'a run) switch =
  if Ocaml_version.Configure_options.is_multicore switch then
    run "opam repo add multicore git+https://github.com/ocaml-multicore/multicore-opam --set-default"
  else
    Dockerfile.empty

let maybe_install_secondary_compiler (run : 'a run) os_family switch =
  let dune_min_native_support = Ocaml_version.Releases.v4_08 in
  (* opam-repository-mingw doesn't package ocaml-secondary-compiler. *)
  if Ocaml_version.compare switch dune_min_native_support < 0 && os_family <> `Windows then
    run "opam install -y ocaml-secondary-compiler"
  else
    Dockerfile.empty

let install_package_archive opam_image =
  let open Dockerfile in
  from ~alias:"opam-archive" "ocaml/opam:archive" @@
  from ~alias:"archive" opam_image @@
  workdir "/home/opam/opam-repository" @@
  run ~mount:(mount_bind ~target:"/cache" ~from:"opam-archive") "rsync -aH /cache/cache/ /home/opam/opam-repository/cache/" @@
  run "opam admin cache --link=/home/opam/opam-repository/cache" @@
  from "alpine:latest" @@
  copy ~chown:"0:0" ~from:"archive" ~src:["/home/opam/opam-repository/cache"] ~dst:"/cache" ()

(* Generate a Dockerfile to install OCaml compiler [switch] in [opam_image]. *)
let install_compiler_df ~distro ~arch ~switch ?windows_port opam_image =
  let switch_name = Ocaml_version.(with_just_major_and_minor switch |> to_string) in
  let (package_name, package_version) =
    match windows_port with
    | None -> Ocaml_version.Opam.V2.package switch
    | Some port -> Windows.ocaml_for_windows_package_exn ~switch ~arch ~port
  in
  let open Dockerfile in
  let os_family = Distro.os_family_of_distro distro in
  let personality = Distro.personality os_family arch in
  let run, run_no_opam, depext, opam_exec =
    let open Windows in
    let bitness = if Ocaml_version.arch_is_32bit arch then "--32" else "--64" in
    match windows_port with
    | None ->
      (fun fmt -> run fmt), (fun fmt -> run fmt), "opam-depext", ["opam"; "exec"; "--"]
    | Some `Msvc ->
      (fun fmt -> run_ocaml_env [bitness; "--ms"] fmt),
      (fun fmt -> run_ocaml_env [bitness; "--ms"; "--no-opam"] fmt),
       "depext", ["ocaml-env"; "exec"; bitness; "--ms"; "--"]
    | Some `Mingw ->
      (fun fmt -> run_ocaml_env [bitness] fmt),
      (fun fmt -> run_ocaml_env [bitness; "--no-opam"] fmt),
      "depext depext-cygwinports", ["ocaml-env"; "exec"; bitness; "--"]
  in
  let shell = maybe (fun pers -> shell [pers; "/bin/sh"; "-c"]) personality in
  let packages =
    let additional_packages = Ocaml_version.Opam.V2.additional_packages switch in
    String.concat "," (Printf.sprintf "%s.%s" package_name package_version :: additional_packages)
  in
  (match os_family with
   | `Linux -> parser_directive (`Syntax "docker/dockerfile:1")
   | `Windows | `Cygwin -> parser_directive (`Escape '`')) @@
  from opam_image @@
  shell @@
  run_no_opam "opam repo add opam-repository-archive git+https://github.com/ocaml/opam-repository-archive --set-default" @@
  maybe_add_overlay run distro switch @@
  maybe_add_beta run switch @@
  maybe_add_multicore run switch @@
  env ["OPAMYES", "1";
       "OPAMCONFIRMLEVEL", "unsafe-yes";
       "OPAMERRLOGLEN", "0"; (* Show the whole log if it fails *)
       "OPAMPRECISETRACKING", "1"; (* Mitigate https://github.com/ocaml/opam/issues/3997 *)
      ] @@
  Dockerfile_opam.ocaml_depexts distro switch @@
  run_no_opam "opam switch create %s --packages=%s" switch_name packages @@
  run "opam pin add -k version %s %s" package_name package_version @@
  run "opam install -y %s" depext @@
  maybe_install_secondary_compiler run os_family switch @@
  entrypoint_exec (Option.to_list personality @ opam_exec) @@
  (match os_family with
   | `Linux ->
     cmd "bash" @@
     copy ~link:true ~src:["Dockerfile"] ~dst:"/Dockerfile.ocaml" ()
   | `Windows ->
     cmd_exec ["cmd.exe"] @@
     copy ~src:["Dockerfile"] ~dst:"/Dockerfile.ocaml" ()
   | `Cygwin -> failwith "No support for Cygwin currently.")

let or_die = function
  | Ok x -> x
  | Error (`Msg m) -> failwith m

let maybe_add_overlay distro hash =
  match distro with
  | `WindowsServer (`Msvc, _)
  | `Windows (`Msvc, _) ->
    Dockerfile.run "opam repo add ocurrent-overlay git+https://github.com/ocurrent/opam-repository-mingw#%s --set-default" hash
  | _ -> Dockerfile.empty

module Make (OCurrent : S.OCURRENT) = struct
  open OCurrent
  open Current.Syntax

  (* Pipeline to build the opam base image and the compiler images for a particular architecture. *)
  module Arch = struct
    (* 2020-04-29: On Windows, squashing images is still experimental (broken). *)
    let squash distro =
      Distro.os_family_of_distro distro <> `Windows

    (* 2022-07-18: Windows Containers don't support BuildKit.
       https://github.com/microsoft/Windows-Containers/issues/34 *)
    let buildkit distro =
      Distro.os_family_of_distro distro <> `Windows

    let install_opam ~arch ~ocluster ~distro ~repos ~push_target ~windows_version () =
      let arch_name = Ocaml_version.string_of_arch arch in
      let distro_tag, os_family = Distro.(tag_of_distro distro, os_family_of_distro distro) in
      Current.component "%s@,%s" distro_tag arch_name |>
      let> { Git_repositories.opam_repository_master;
             opam_repository_mingw_sunset;
             opam_overlays;
             opam_2_0;
             opam_2_1;
             opam_2_2;
             opam_2_3;
             opam_master
           } = repos
      and> windows_version = windows_version in
      let dockerfile =
        let opam_hashes = {
          Dockerfile_opam.opam_2_0_hash = Current_git.Commit_id.hash opam_2_0;
          opam_2_1_hash = Current_git.Commit_id.hash opam_2_1;
          opam_2_2_hash = Current_git.Commit_id.hash opam_2_2;
          opam_2_3_hash = Current_git.Commit_id.hash opam_2_3;
          opam_master_hash = Current_git.Commit_id.hash opam_master;
        } in
        `Contents (
          let opam = snd @@ match windows_version with
          | "" -> Dockerfile_opam.gen_opam2_distro ~arch ~clone_opam_repo:false ~opam_hashes distro
          | override_tag -> Dockerfile_opam.gen_opam2_distro ~arch ~override_tag ~clone_opam_repo:false ~opam_hashes distro
          in
          let open Dockerfile in
          string_of_t (
            begin match os_family with
            | `Linux ->
              opam @@
              copy ~link:true ~chown:"opam:opam" ~src:["."] ~dst:"/home/opam/opam-repository" () @@
              run "opam-sandbox-disable" @@
              run "opam init -k local -a /home/opam/opam-repository --bare" @@
              run "rm -rf .opam/repo/default/.git" @@
              copy ~link:true ~src:["Dockerfile"] ~dst:"/Dockerfile.opam" ()
            | `Windows ->
              opam @@
              let opam_repo = Windows.Cygwin.default.root ^ {|\home\opam\opam-repository|} in
              let opam_root = {|C:\opam\.opam|} in
              copy ~src:["."] ~dst:opam_repo () @@
              env [("OPAMROOT", opam_root)] @@
              run "opam init -k local -a \"%s\" --bare --disable-sandboxing" opam_repo @@
              maybe_add_overlay distro (Current_git.Commit_id.hash opam_overlays) @@
              Windows.Cygwin.run_sh "rm -rf /cygdrive/c/opam/.opam/repo/default/.git" @@
              copy ~src:["Dockerfile"] ~dst:"/Dockerfile.opam" ()
            | `Cygwin -> failwith "No support for Cygwin currently."
            end)
        )
      in
      let options = { Cluster_api.Docker.Spec.defaults with
                      squash = squash distro;
                      buildkit = buildkit distro;
                      include_git = true } in
      let cache_hint = Printf.sprintf "opam-%s" distro_tag in
      let opam_repository = match os_family with `Windows -> opam_repository_mingw_sunset | _ -> opam_repository_master in
      OCluster.Raw.build_and_push ocluster ~src:[opam_repository] dockerfile
        ~cache_hint
        ~options
        ~push_target
        ~pool:(Conf.pool_name distro arch)

    let install_compiler ~distro ~arch ~ocluster ~switch ~push_target ?windows_port base =
      let arch_name = Ocaml_version.string_of_arch arch in
      Current.component "%s/%s" (Ocaml_version.to_string switch) arch_name |>
      let> base = base in
      let dockerfile = `Contents (install_compiler_df ~distro ~arch ~switch ?windows_port base |> Dockerfile.string_of_t) in
      (* ([include_git] doesn't do anything here, but it saves rebuilding during the upgrade) *)
      let options = { Cluster_api.Docker.Spec.defaults with
                      squash = squash distro;
                      buildkit = buildkit distro;
                      include_git = true } in
      let cache_hint = Printf.sprintf "%s-%s-%s" (Ocaml_version.to_string switch) arch_name base in
      OCluster.Raw.build_and_push ocluster ~src:[] dockerfile
        ~cache_hint
        ~options
        ~push_target
        ~pool:(Conf.pool_name distro arch)

    let collect_archive ~distro ~ocluster ~push_target base =
      Current.component "archive" |>
      let> base = base in
      let dockerfile = `Contents (install_package_archive base |> Dockerfile.string_of_t) in
      let options = { Cluster_api.Docker.Spec.defaults with
                      squash = squash distro;
                      buildkit = buildkit distro;
                      include_git = true } in
      let cache_hint = Printf.sprintf "archive-%s" base in
      OCluster.Raw.build_and_push ocluster ~src:[] dockerfile
        ~cache_hint
        ~options
        ~push_target
        ~pool:(Conf.pool_name distro `X86_64)

    (* Build the base image for [distro], plus an image for each compiler version. *)
    let pipeline ~ocluster ~repos ?(windows_version = Windows_map.empty) ~distro arch =
      let update_index current distro switch =
        let+ state = Current.state ~hidden:false current in
        let s = match state with
        | Ok _ -> Index.Ok
        | Error (`Active _) -> Active
        | Error (`Msg _) -> Failed
        in
        Index.update_images_per_platform
          ~platform:(Distro.human_readable_string_of_distro distro)
          ~switch s
      in
      let opam_image =
        let push_target =
          Tag.v distro ~arch
          |> Cluster_api.Docker.Image_id.of_string
          |> or_die
        in
        let windows_version = Windows_map.find_opt distro windows_version |> Option.value ~default:(Current.return "") in
        install_opam ~arch ~ocluster ~distro ~repos ~windows_version ~push_target ()
      in
      let _ = update_index opam_image distro None in
      let compiler_images =
        Conf.switches ~arch ~distro |> List.map @@ fun switch ->
        let push_target =
          Tag.v distro ~switch ~arch
          |> Cluster_api.Docker.Image_id.of_string
          |> or_die
        in
        let windows_port = match distro with
        | `WindowsServer (port, _) -> Some port
        | `Windows (port, _) -> Some port
        | _ -> None in
        let repo_id = install_compiler ~distro ~arch ~ocluster ~switch ~push_target ?windows_port opam_image in
        let _ = update_index repo_id distro (Some switch) in
        (switch, repo_id)
      in
      (* Build the archive image for the debian 10 / x86_64 image only *)
      let archive_image =
        if distro = master_distro && arch = `X86_64 then
          let push_target =
            Tag.archive ~staging:true ()
            |> Cluster_api.Docker.Image_id.of_string
            |> or_die
          in
          Some (collect_archive ~distro ~ocluster ~push_target opam_image)
        else None
      in
      let compiler_images = Switch_map.of_seq (List.to_seq compiler_images) in
      (opam_image, compiler_images, archive_image)
  end

  let all_switches arches =
    let module Switch_set = Set.Make(Ocaml_version) in
    arches |> ListLabels.fold_left ~init:Switch_set.empty ~f:(fun acc map ->
        Switch_map.fold (fun k _v acc -> Switch_set.add k acc) map acc
      )
    |> Switch_set.elements

  let label l t =
    Current.component "%s" l |>
    let> v = t in
    Current.Primitive.const v

  let pipeline ~ocluster ~repos ?windows_version distro gen_tags =
    let opam_images, ocaml_images, archive_image =
      let arch_results =
        let arches = Conf.arches_for ~distro in
        List.map (Arch.pipeline ~ocluster ~repos ~distro ?windows_version) arches in
      List.fold_left (fun (aa,ba,ca) (a,b,c) ->
          let ca = match ca,c with Some v, _ -> Some v | None, v -> v in
          a::aa, b::ba, ca) ([], [], None) arch_results
    in
    let (multiarch_images, ocaml_images) =
      all_switches ocaml_images |> List.filter_map @@ begin fun switch ->
        match List.filter_map (Switch_map.find_opt switch) ocaml_images with
        | [] -> None
        | images ->
           let full_tag = Tag.v distro ~switch in
           let (multiarch_images, tags) = gen_tags images full_tag switch (aliases_of distro) in
           let pushes = List.map (fun tag -> Docker.push_manifest ?auth:Conf.auth ~tag images |> Current.ignore_value) tags in
           Some (multiarch_images, (full_tag, Current.all pushes))
        end |> List.split
    in
    let archive_images =
      match archive_image with
      | None -> []
      | Some image -> [ "archive", Docker.push_manifest ?auth:Conf.auth ~tag:(Tag.archive ()) [image] |> Current.ignore_value ]
    in
    let pipeline =
      Current.all_labelled (
          ("base", Docker.push_manifest ?auth:Conf.auth ~tag:(Tag.v distro) opam_images |> Current.ignore_value)
          :: ocaml_images
          @ archive_images)
    in
    multiarch_images, pipeline

  let linux_pipeline ~ocluster ~repos distro =
    let distro_label = Distro.tag_of_distro distro in
    let repos = label distro_label repos in
    Current.collapse ~key:"distro" ~value:distro_label ~input:repos @@
      let gen_tags _images full_tag switch distro_aliases =
        let tags =
          (* Push the image as e.g. debian-10-ocaml-4.09 and debian-ocaml-4.09 *)
          let tags = full_tag :: List.map (Tag.v ~switch) distro_aliases in
          if switch <> Ocaml_version.Releases.latest then tags
          else (
            (* For every distro, also create a link to the latest OCaml compiler.
               e.g. debian-9 -> debian-9-ocaml-4.09 *)
            let tags = Tag.v_alias distro :: tags in
            (* If [distro] is the latest version of that distribution, make an alias like
               debian -> debian-10-ocaml-4.09 *)
            let tags = List.map Tag.v_alias distro_aliases @ tags in
            (* The top-level alias: latest -> debian-10-ocaml-4.09 *)
            if distro <> master_distro then tags
            else Tag.latest :: tags
          )
        in
        (* Fmt.pr "Aliases: %s -> %a@." full_tag Fmt.(Dump.list string) (List.sort String.compare tags); *)
        Switch_map.empty, tags
      in
      let (_multiarch_images, pipeline) = pipeline ~ocluster ~repos distro gen_tags in
      pipeline

  let windows_distro_pipeline ~ocluster ~repos ~windows_version distro_label distro_versions =
    let distro_pipeline multiarches distro =
      let gen_tags images full_tag switch distro_aliases =
        let tags =
          (* Push the image as e.g. windows-mingw-1809-ocaml-4.09 and windows-mingw-ltsc2019-ocaml-4.09  *)
          let tags = full_tag :: List.map (Tag.v ~switch) distro_aliases in
          if switch <> Ocaml_version.Releases.latest then tags
          else (
            (* For every distro, also create a link to the latest OCaml compiler.
               e.g. windows-mingw-1809 -> windows-mingw-1809-ocaml-4.09 *)
            Tag.v_alias distro :: tags
          )
        in
        (* Fmt.pr "Aliases: %s -> %a@." full_tag Fmt.(Dump.list string) (List.sort String.compare tags); *)
        Switch_map.add switch images Switch_map.empty, tags
      in
      let (multiarch_images, pipeline) = pipeline ~ocluster ~repos ~windows_version distro gen_tags in
      let multiarch_images =
        let update images = function None -> Some images | Some images' -> Some (images @ images') in
        let fold switch images acc = Switch_map.update switch (update images) acc in
        let fold_left acc images = Switch_map.fold fold acc images in
        List.fold_left fold_left multiarches multiarch_images
      in
      (multiarch_images, pipeline)
    in
    let multiarch, pipelines = List.fold_left_map distro_pipeline Switch_map.empty distro_versions in
    let pushes =
      Switch_map.fold (fun switch images pushes ->
        let distro_label = String.split_on_char '-' distro_label |> String.concat "-all-" in
        let full_tag = Printf.sprintf "%s:%s-ocaml-%s" Conf.public_repo distro_label (Tag.tag_of_compiler switch) in
        let tags =
          let tags = [full_tag] in
          if switch <> Ocaml_version.Releases.latest then tags
          else Printf.sprintf "%s:%s" Conf.public_repo distro_label :: tags
        in
        (* Fmt.pr "Aliases: %s -> %a@." full_tag Fmt.(Dump.list string) (List.sort String.compare tags); *)
        let pushes' = List.map (fun tag -> Docker.push_manifest ?auth:Conf.auth ~tag images |> Current.ignore_value) tags in
        (full_tag, Current.all pushes') :: pushes)
      multiarch []
    in
    Current.(collapse ~key:"distro" ~value:distro_label ~input:repos (all ((all_labelled pushes) :: pipelines)))

  let windows_pipeline ~ocluster ~repos ~windows_version mingw msvc cygwin =
    List.filter_map (fun (distro_label, distros) ->
        match distros with
        | [] -> None
        | distro_versions ->
           let repos = label distro_label repos in
           windows_distro_pipeline ~ocluster ~repos ~windows_version distro_label distro_versions |> Option.some)
      [("windows-mingw", mingw); ("windows-msvc", msvc); ("cygwin", cygwin)]

  (* The main pipeline. Builds images for all supported distribution, compiler version and architecture combinations. *)
  let v ~ocluster ~repos ~windows_version =
    let linux, mingw, msvc, cygwin = Conf.distros |> List.fold_left (fun (linux, mingw, msvc, cygwin) distro ->
      let os_family = Distro.os_family_of_distro distro in
      match os_family with
      | `Linux -> distro :: linux, mingw, msvc, cygwin
      | `Cygwin -> linux, mingw, msvc, distro :: cygwin
      | `Windows ->
         match distro with
         | `WindowsServer (`Mingw, _)
         | `Windows (`Mingw, _) -> linux, distro :: mingw, msvc, cygwin
         | `WindowsServer (`Msvc, _)
         | `Windows (`Msvc, _) -> linux, mingw, distro :: msvc, cygwin
         | _ -> assert false) ([], [], [], [])
    in
    let pipelines =
      List.rev_map (linux_pipeline ~ocluster ~repos) linux
      @ windows_pipeline ~ocluster ~repos ~windows_version mingw msvc cygwin in
    Current.all pipelines
end

module Real = Make(struct
    module Current = Current
    module OCluster = Current_ocluster
    module Docker = Current_docker
  end)

open Current.Syntax

let notify_status ?channel x =
  match channel with
  | None -> x
  | Some channel ->
    let s =
      let+ state = Current.catch x in
      Fmt.str "docker-base-images status: %a" (Current_term.Output.pp Current.Unit.pp) state
    in
    Current.all [
      Current_slack.post channel ~key:"base-images-status" s;
      x   (* If [x] fails, the whole pipeline should fail too. *)
    ]

let v ?channel ~connection ~ocluster () =
  if Conf.auth = None then Fmt.pr "Password file %S not found; images will not be pushed to hub@." Conf.password_path;
  let repos = git_repositories () in
  let wv = win_ver connection  in
  Real.v ~ocluster ~repos ~windows_version:wv |> notify_status ?channel
