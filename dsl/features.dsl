// dsl/features.dsl — minimal regression fixture for the DSL's sugar:
//   def refs (zero-arg KL calls), backquote grouping, and ,@ splicing.
(def greeting "hi")

(template feature_box (xs)
  `(div (@ class "box") ,greeting ,@xs))