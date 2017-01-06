type attribute = string * string

type element = string * (attribute array)

type node =
  | Element of element * node list
  | VoidElement of element
  | Text of string
  | WhiteSpace of string

let tag tag attr (contents : node list) = Element ((tag, Array.of_list attr), contents)
let voidTag tag attr = VoidElement (tag, Array.of_list attr)
let text s = [Text s]

let emptyText = text ""
let html = tag "html"
let head = tag "head"
let title attr s = tag "title" attr [Text s]
let script = tag "script"
let body = tag "body"
let div = tag "div"
let p = tag "p"
let a href attr = tag "a" (("href", href)::attr)
let span = tag "span"

let link attr = voidTag "link" attr
let meta attr = voidTag "meta" attr
let hr attr = voidTag "hr" attr
let br attr = voidTag "br" attr
let img attr = voidTag "img" attr
let input attr = voidTag "input" attr

let startElemToString (e, attributes) =
  match attributes with
  | [||] -> Printf.sprintf "<%s>" e
  | xs ->
    let attributeString =
      attributes
      |> Array.map (fun (k,v) -> Printf.sprintf " %s=\"%s\"" k v)
      |> Array.to_list
      |> String.concat " "
    in Printf.sprintf "<%s%s>" e attributeString

let endElemToString (e, _) = Printf.sprintf "</%s>" e

let rec htmlToString node =
  match node with
  | Text text -> text
  | WhiteSpace text -> text
  | Element (e, nodes) ->
    let inner = nodes |> List.map htmlToString |> String.concat " "in
    let startTag = e |> startElemToString in
    let endTag = e |> endElemToString in
    Printf.sprintf "%s%s%s" startTag inner endTag
  | VoidElement e -> e |> startElemToString
