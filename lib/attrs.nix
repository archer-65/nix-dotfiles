{ lib, ... }:

with builtins;
with lib; {
  # Utility function that converts an attrset to a list of {name, value} pairs.
  # Inverse of builtins.listToAttrs: listToAttrs (atrsToList attrs) == attrs
  attrsToList = mapAttrsToList nameValuePair;
}
