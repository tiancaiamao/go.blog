open Lwt
open Cohttp
open Cohttp_lwt_unix
open View
open Yojson

type entity = {title: string; date: string; tags: string array option; category: string option; file: string}

let tag_field json = (try json |> Basic.Util.member "Tags" |> Basic.Util.to_list with _ -> [])
                     |> List.map Basic.Util.to_string
                     |> function
                     | [] -> None
                     | x ->  Some (Array.of_list x)

let convert_to_entity json =
  let title = json |> Basic.Util.member "Title" |> Basic.Util.to_string in
  let date = json |> Basic.Util.member "Date" |> Basic.Util.to_string in
  let tags = json |> tag_field in
  let category = json |> Basic.Util.member "Category" |> Basic.Util.to_string_option in
  let file = json |> Basic.Util.member "File" |> Basic.Util.to_string in
  {title=title; date=date; tags=tags; category=category; file=file}

let entities =
  Basic.from_file "../content/index.json"
  |> Basic.Util.to_list
  |> List.map convert_to_entity

let has_suffix suffix str =
  (String.length str > String.length suffix) &&
  (String.sub str (String.length str - String.length suffix) (String.length suffix)  = suffix)

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  s

let post_content title date content=
  View.container
    [Html.tag "h1" ["id","Title"] (Html.text title);
     Html.p [] (Html.text date);
     Html.Text content;]
    Html.emptyText

let load_md (e : entity) =
  let base = String.sub e.file 0 ((String.length e.file)-3) in
  let file = "../generated/" ^ base ^ ".out" in
  let content = load_file file in
  View.page e.title (post_content e.title e.date content);;

let build_index (entities : entity list) =
  let hash = Hashtbl.create 300 in
  entities
  |> List.filter (fun x -> (has_suffix ".md" x.file))
  |> List.iter (fun x -> Hashtbl.add hash x.file (load_md x));
  hash

let build_category (entities : entity list) =
  let hash = Hashtbl.create 300 in
  entities
  |> List.iter (fun x -> match x.category with
      | None -> ()
      | Some c -> if Hashtbl.mem hash c
        then Hashtbl.replace hash c (x::(Hashtbl.find hash c))
        else Hashtbl.add hash c [x]);
  hash

let iNDEX = build_index entities

let home_handler conn req body =
  let str = container Html.emptyText Html.emptyText
            |> page "test" |> Html.htmlToString in
  Server.respond_string `OK str ()

let md_handler conn uri body =
  try
    let html = Hashtbl.find iNDEX (String.sub uri 1 ((String.length uri)-1)) in
    let str = html |> Html.htmlToString in
    Server.respond_string `OK str ()
  with _ -> Server.respond_string `OK uri ()

let blog_handler conn req body =
  let blog = (View.container
                (entities
                 |> List.map (fun x -> View.blog x.title x.file x.date)
                 |> List.flatten)
                Html.emptyText)
             |> page "blog"
             |> Html.htmlToString
  in Server.respond_string `OK blog ()

let about_handler conn req body =
  let file = load_file "../generated/about.out" in
  let content = container (Html.text file) Html.emptyText
            |> page "about"
            |> Html.htmlToString
  in Server.respond_string `OK content ()

let router conn req body =
  let uri = req |> Request.uri |> Uri.path in
  match uri with
  | "/" -> blog_handler conn req body
  | "/index" -> blog_handler conn req body
  | "/about" -> about_handler conn req body
  (* | "/feed.atom" -> atom_handler *)
  | _ -> if has_suffix ".md" uri
      then md_handler conn uri body
      else Server.respond_string `OK uri ()

let server = Server.create (Server.make router ())

let () = ignore (Lwt_main.run server)
