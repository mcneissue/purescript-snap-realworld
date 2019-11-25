module FakeData where

import Prelude

import Types (Article, Profile, Tag)

profiles :: { eric :: Profile, albert :: Profile }
profiles =
  { eric:   { url: "", picture: "http://i.imgur.com/Qr71crq.jpg", name: "Eric Simons" }
  , albert: { url: "", picture: "http://i.imgur.com/N4VcUeJ.jpg", name: "Albert Pai" }
  }

articles :: Array Article
articles =
  [ { title: "How to build webapps that scale"
    , description: "This is the description for the post."
    , hearts: 29
    , date: "January 20th"
    , author: profiles.eric
    , url: ""
    , body: "\
            \<p>\
            \  Web development technologies have evolved at an incredible clip\
            \  over the past few years.\
            \</p>\
            \<h2 id=\"introducing-ionic\">Introducing RealWorld.</h2>\
            \<p>It's a great solution for learning how other frameworks work.</p>\
            "
    }

  , { title: "The song you won't ever stop singing. No matter how hard you try"
    , description: "This is the description for the post"
    , hearts: 32
    , date: "January 20th"
    , author: profiles.albert
    , url: ""
    , body: "\
            \<p>\
            \  Look at me I am say things too\
            \</p>\
            "
    }
  ]

tags :: Array Tag
tags = { url: "", name: _ } <$> ["programming", "javascript", "emberjs", "angularjs", "react", "mean", "node", "rails"]
