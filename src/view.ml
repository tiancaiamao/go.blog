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
