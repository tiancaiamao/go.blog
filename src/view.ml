open Html

let ul attr li = tag "ul" attr li
let li n = tag "li" [] [n]
let aHref href str = tag "a" ["href", href] [Text str]

let footbar = p ["class", "nav navbar-nav navbar-right"] [
    (Text "Powered by ");
    (aHref "http://www.suave.io/" "Suave");
    (Text " & ");
    (aHref "http://getbootstrap.com/" "Bootstrap");
  ];;

let page titleStr container =
  html ["lang", "zh_CN"] [
    head [] [
      meta ["charset", "utf-8"];
      title [] titleStr;
      link [("rel", "stylesheet");
            ("href", "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css")
           ]
    ];

    body [] [
      div ["class", "navbar navbar-inverse"] [
        div ["class", "container"] [
          div ["class", "nav navbar-nav"] [
            ul ["class", "nav navbar-nav"] [
              li (aHref "/" "Home");
              li (aHref "/index" "Blog");
              li (aHref "/about" "About");
              li (aHref "/feed.atom" "Rss")
            ]
          ]
        ]
      ];

      container;

      hr [];

      footbar;
    ]
  ];;

let container content sidebar =
  div ["class", "container"] [
    div ["class", "row row-offcanvas row-offcanvas-right"] [
      div ["class", "col-sm-9"] content;
      div [("class", "col-sm-3");
           ("id", "sidebar");
           ("role", "navigation")] sidebar
    ]
  ];;

let blog title file date =
  [tag "h2" [] [aHref file title];
   div ["class", "meta"] (text date)]

let feed attr li = tag "feed" attr li
let autohr = tag "author" []
let author li = tag "author" [] li
let name s = tag "name" [] (text s)
let email s = tag "email" [] (text s)
let uri s = tag "uri" [] (text s)
let id s = tag "id" [] (text s)
let updated s = tag "updated" [] (text s)
let published s = tag "published" [] (text s)
let title s = tag "title" ["type","text"] (text s)
let rights s = tag "rights" ["type","text"] (text s)
let subtitle s = tag "subtitle" ["type","text"] (text s)

let feed entries = tag "feed" ["xmlns","http://www.w3.org/2005/Atom"] ([
    author [
       name "Arthur";
       email "tiancaiamao@gmail.com";
    ];
    id "www.zenlife.tk";
    tag "generator" ["uri","http://3e8.org/chickadee/atom";"version","0.1.1"] (text "atom egg for Chicken");
    tag "link" [("href","http://www.zenlife.tk/feed.atom");
          ("rel","self");
          ("type", "application/atom+xml")] [];
    rights  "Copyright (c) 2015, Arthur";
    subtitle "伟大的野心家，实践家";
    title "Arthur的博客";
    updated "2005-07-31T12:29:29Z";
] @ entries);;
