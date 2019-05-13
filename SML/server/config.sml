(* fun parserConfigFile (data: string) =
  let fun parse index data =
        if index = string.size data then

fun readConfigFile fileName =
  let val fileInStream = TextIO.openIn fileName
      val allBytes = StreamIO.input fileInStream
      val strings = Bytes.bytesToString allBytes
  in parserConfigFile strings
  end *)
fun parserArguments args =
  let val tunnel = Option.isSome (List.find (fn s => String.compare(s, "tunnel") = EQUAL) args)
  in
    tunnel
  end

val tunnelMode = parserArguments (CommandLine.arguments ())
