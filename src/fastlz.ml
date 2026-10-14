[@@@ai_disclosure "ai-generated"]
[@@@ai_provider "Anthropic, OpenAI"]

(* FastLZ levels 1 and 2 (https://github.com/ariya/FastLZ), as bundled with libfst; the level is
   in the top three bits of the first byte. *)

exception Corrupt

let decompress_into s ~off ~len dst ~dst_off ~dst_len =
  if len < 1 then None
  else
    let level = (Char.code s.[off] lsr 5) + 1 in
    if level > 2 then None
    else
      let ip = ref off and ip_limit = off + len in
      let op = ref dst_off and op_limit = dst_off + dst_len in
      let byte () =
        if !ip >= ip_limit then raise Corrupt;
        let b = Char.code (String.unsafe_get s !ip) in
        incr ip;
        b
      in
      try
        let ctrl = ref (byte () land 31) in
        let loop = ref true in
        while !loop do
          let c = !ctrl in
          if c >= 32 then begin
            let n = ref ((c lsr 5) - 1) in
            let ofs = (c land 31) lsl 8 in
            let r = ref (!op - ofs) in
            if !n = 6 then
              if level = 1 then n := !n + byte ()
              else begin
                let code = ref 255 in
                while !code = 255 do
                  code := byte ();
                  n := !n + !code
                done
              end;
            let code = byte () in
            r := !r - code;
            if level = 2 && code = 255 && ofs = 31 lsl 8 then begin
              let hi = byte () in
              let lo = byte () in
              r := !op - ((hi lsl 8) lor lo) - 8191
            end;
            if !op + !n + 3 > op_limit || !r - 1 < dst_off then raise Corrupt;
            if !ip < ip_limit then ctrl := byte () else loop := false;
            let src = ref (!r - 1) in
            for _ = 1 to !n + 3 do
              Bytes.unsafe_set dst !op (Bytes.unsafe_get dst !src);
              incr op;
              incr src
            done
          end
          else begin
            let n = c + 1 in
            if !op + n > op_limit || !ip + n > ip_limit then raise Corrupt;
            Bytes.blit_string s !ip dst !op n;
            ip := !ip + n;
            op := !op + n;
            if !ip < ip_limit then ctrl := byte () else loop := false
          end
        done;
        Some (!op - dst_off)
      with Corrupt -> None
