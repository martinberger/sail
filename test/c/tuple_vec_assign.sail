default Order dec

$include <prelude.sail>

register X : bits(4)
register Y : bits(4)

function main() -> unit = {
  var c: bool = false;
  (c, X @ Y) = (true, 0xFF);
  print_bits("X = ", X);
  print_bits("Y = ", Y)
}
