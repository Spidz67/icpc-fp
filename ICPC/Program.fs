module ICPC
open System
open System

let commaSprinkler input =
    // method to check for capial letters
    let CheckForCapitalLetters (str:string) = 
        let strChars = str.ToCharArray()
        let strChar = List.ofArray(strChars) 
        //let Capitals = ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z']
        let rec CheckEach (strChar:List<char>) = 
            match Char.IsLower(strChar.Head) with
            | false -> None
            | _-> CheckEach strChar.Tail
        CheckEach strChar
    
    // method to check for question marks ...
    let CheckQuestionMark (str:string) = 
        let strChars = List.ofArray(str.ToString().ToCharArray())
        match List.contains '?' strChars with
        | true -> None
        | _ -> Some ()

    // method to check that the last word of the text is followed by a period with no trailing space... 
    let CheckLastPeriod str = 
        let strChars = List.ofArray(str.ToString().ToCharArray())
        match List.nth strChars (strChars.Length-1) with
        | '.' -> Some () 
        | _-> None

    // method to check that the text begins with a word
    let CheckTextBeginWithWord str = 
        let strChars = List.ofArray(str.ToString().ToCharArray())
        match strChars.Head with 
        | 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' -> Some ()
        | _-> None

    // method to check that a string is two characters long or more...
    let CheckHowManyChars (str:string) =
        match str.Length with 
        | 0 | 1 -> None
        | _-> Some ()

    // 1. CheckHowManyChars input
    // 2. CheckTextBeginWithWord input
    // 3. CheckLastPeriod input 
    // 4. CheckQuestionMark input
    // 5. CheckForCapitalLetters input

    // checks all errors at once...
    let CheckErrors input = 
        match CheckHowManyChars input with 
        | None -> true
        | _ -> match CheckTextBeginWithWord input with 
                | None -> true 
                | _ -> match CheckLastPeriod input with 
                        | None -> true 
                        | _ -> match CheckQuestionMark input with 
                               | None -> true  
                               | _ -> match CheckForCapitalLetters input with 
                                      | None -> true 
                                      | _ -> false
    
    // get index of the first comma 
    let GetIndexOfComma (str:string) = 
        let idx = str.IndexOf(',')
        idx 
    
    // get string before the comma 
    let GetStringBefore (str:string) index =
        let SubBefore = str.Substring(0, index-1)
        let ListSubBefore = SubBefore.Split()
        let wordBefore = ListSubBefore.GetValue(ListSubBefore.Length-1)
        wordBefore 

    // get string after the comma 
    let GetStringAfter (str:string) index = 
        let SubAfter = str.Substring(0, index-1)
        let ListSubAfter = SubAfter.Split()
        let wordAfter = ListSubAfter.GetValue(ListSubAfter.Length-1)
        wordAfter

    // method to check that the given word is not the end of the sentence...
    let CheckIsNotLastWord (str:string) (word:string) =
        let idx = str.IndexOf(word)
        match str.Chars(idx+1) with
        | '.' -> None
        | _ -> Some ()

    // method to put a comma after the given word, provided it's not the last word of the sentence...
    
    // method to put a comma before the given word, provided it's not the beginnnig of the sentence... 
    

    match CheckErrors input with 
    | true -> None
    | _ -> Some ()

let rivers input =
    // method to check that a string is two characters long or more...
    let CheckHowManyChars (str:string) =
        match str.Length with 
        | 0 | 1 -> None
        | _-> Some ()

    // method to check that all characters are lower or upper cases...
    let CheckForLowerOrUpperLetters (str:string) = 
        let strChars = str.ToCharArray()
        let strChar = List.ofArray(strChars) 
        let rec CheckEach (strChar:List<char>) = 
            match strChar.IsEmpty with
            | true -> Some()
            | _ -> match (Char.IsLower(strChar.Head), Char.IsUpper(strChar.Head)) with
                   | false,false -> None
                   | _ -> CheckEach strChar.Tail
        CheckEach strChar

    // method to check if spaces are not next to each other...
    // we know that spaces in the text should be (string.lenght-1) in lenght...
    let CheckSpaces (str:string) = 
        let strChars = List.ofArray(str.ToCharArray())
        let rec CountSpaces (strChars:List<char>) counter =
            match Char.IsWhiteSpace(strChars.Head) with 
            | true -> CountSpaces strChars.Tail (counter+1) 
            | _ -> CountSpaces strChars.Tail counter
        CountSpaces strChars 0 
    
    // method to check if we not starting or ending with a space...
    let CheckSpacesOnStartAndEnd (str:string) = 
        let strChars = List.ofArray(str.ToCharArray())
        match (strChars.Head, List.nth strChars (strChars.Length-1)) with 
        | ' ',' ' -> None
        | ' ', _ -> None
        | _, ' ' -> None
        | _ -> Some ()

    // method to check whether each word is not more than 80 characters...
    let CheckCharsForEachWord (str:string) = 
        let strStrings = List.ofArray(str.Split())
        let rec CheckHowManyChars (strStrings:List<string>) = 
            match strStrings.Head.Length > 80 with 
            | true -> None
            | _ -> CheckHowManyChars strStrings
        CheckHowManyChars strStrings
    
    // method to check if we have 2 or more words in the string...
    let CheckHowManyWords (str:string) = 
        let strStrings = List.ofArray(str.Split())
        match strStrings.Length<2 with 
        | true -> None
        | _ -> Some()

    // 1. CheckHowManyChars input
    // 2. CheckHowManyWords input 
    // 3. CheckForLowerOrUpperLetters input 
    // 4. CheckSpacesOnStartAndEnd input 
    // 5. CheckSpaces input 
    // 6. CheckCharsForEachWord input 

    // checks all errors at once...
    let CheckErrors input = 
        match CheckHowManyChars input with 
        | None -> true 
        | _ -> match CheckHowManyWords input with 
               | None -> true
               | _ -> match CheckForLowerOrUpperLetters input with 
                      | None -> true 
                      | _ -> match CheckSpacesOnStartAndEnd input with 
                             | None -> true 
                             | _ -> match CheckSpaces input with 
                                    | None -> true 
                                    | _ -> match CheckCharsForEachWord input with 
                                           | None -> true 
                                           | _ -> false
        
    match CheckErrors input with 
    | true -> None
    | _ -> Some()

    // function to get the number of the largest word
    //let GetLongestWordNumber (str:string) =
      //  let strStrings = List.ofArray(str.Split())
        //let rec GetNumber (strStrings:List<string>) LongNumber = 
          //  match strStrings.Head.Length > LongNumber with 
            //| true -> GetNumber strStrings.Tail strStrings.Head.Length
            //| _ -> strStrings.Head.Length
        //GetNumber strStrings 0

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code