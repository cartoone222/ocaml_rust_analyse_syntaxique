open AnalyseurDeBase

let () =
  let result = pure 42 "hello" in
  assert (result = [ (42, "hello") ]);

  let result = empty "hello" in
  assert (result = []);

  let result = (empty <|> pure 42) "hello" in
  assert (result = [ (42, "hello") ]);

  let result = (pure 42 <|> empty) "hello" in
  assert (result = [ (42, "hello") ]);

  let result = (pure 42 <|> pure 43) "hello" in
  assert (result = [ (42, "hello") ]);

  let result = item "hello" in
  assert (result = [ ('h', "ello") ]);

  let parser = item >>= fun c -> pure (Char.uppercase_ascii c) in
  assert (parser "abc" = [ ('A', "bc") ]);

  let parser = sat (fun c -> c == '0') in
  assert (parser "0abc" = [ ('0', "abc") ]);

  let parser = sat (fun c -> c == '0') in
  assert (parser "abc" = [ ]);

  let parser = char '0' in
  assert (parser "0abc" = [ ('0', "abc") ]);

  let parser = char '0' in
  assert (parser "abc" = [ ]);

  let parser = char '0' >> char 'a' in
  assert (parser "0abc" = [ ('a', "bc") ]);

  let parser = char '0' >> char 'a' in
  assert (parser "abc" = [ ]);

  let parser = string "0abc" in
  assert (parser "0abc" = [ ("0abc", "") ]);

  let parser = string "0" in
  assert (parser "abc" = [ ]);

  let parser = string "0ab" in
  assert (parser "0abc" = [ ("0ab", "c") ]);

  let parser = string "0abc" in
  assert (parser "" = [ ]);

  let parser = many (char 'x') in
  assert (parser "0abc" = [ ( [] , "0abc") ]);

  let parser = some (char 'x') in
  assert (parser "0abc" = [ ]);

  let parser = many (char 'x') in
  assert (parser "xxx0abc" = [ ( ['x'; 'x'; 'x'] , "0abc") ]);

  let parser = some (char 'x') in
  assert (parser "xxx0abc" = [ ( ['x'; 'x'; 'x'] , "0abc") ]);

  print_endline "Test OK"