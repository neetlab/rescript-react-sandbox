type vector<'a> = Vector('a, 'a, 'a)

let norm = (a: vector<'a>): float => {
  open Js.Math
  open Belt.Int
  let Vector(x, y, z) = a
  hypotMany([x->toFloat, y->toFloat, z->toFloat])
}

let add = (a: vector<'a>, b: vector<'a>): vector<'a> => {
  let Vector(x1, y1, z1) = a
  let Vector(x2, y2, z2) = b
  Vector(x1 + x2, y1 + y2, z1 + z2)
}

let dotProd = (a: vector<'a>, b: vector<'a>): 'a => {
  let Vector(x1, y1, z1) = a
  let Vector(x2, y2, z2) = b
  x1 * x2 + y1 * y2 + z1 * z2
}

let angle = (a: vector<'a>, b: vector<'a>): float => {
  open Belt.Int
  open! Belt.Float
  let prod = dotProd(a, b)->toFloat
  let na = a->norm
  let nb = b->norm
  Js.Math.acos(prod / na * nb)
}

let a = Vector(1, 0, 0)
let b = Vector(0, 1, 0)
Js.log(dotProd(a, b) == 0)
Js.log(angle(a, b))
