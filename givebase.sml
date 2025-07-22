fun minbases(nums : int list) =
    let
        fun checkSameDigits(num, base) =
            let
                fun digits(n, acc) =
                    if n = 0 then acc
                    else digits(n div base, n mod base :: acc)
                
                val digitList = digits(num, [])
                val firstDigit = hd digitList
            in
                List.all (fn d => d = firstDigit) digitList
            end;
        
        fun findMinBase(num) =
            let
                fun loop(base) =
                    if checkSameDigits(num, base) then base
                    else loop(base + 1)
            in
                loop 2
            end;
        
        val minBases = List.map findMinBase nums
    in
        minBases
    end;
