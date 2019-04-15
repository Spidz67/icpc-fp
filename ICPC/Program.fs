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

    

    let sInput = string input
    let myArr = sInput.Split()
    let myList = Array.toList(myArr)
    let listTostring (list:string list) =
        let func state item = sprintf "%s %s" state item
        List.fold func "" list

   
    let lent = myList.Length
    match List.tryFind (fun x -> x=""||x=" "||x=" "||x="."||x =",") myList with
     |Some a -> None
     |None -> 
             let myFunc x =     
                let word = string x
                word.EndsWith(',')
             let front = List.filter myFunc myList
             let front = List.distinct front
             let rec sprinkleIt (comfront : string list) (comback  : string list) (theList : string list) =
                match comfront.Length = 0, comback.Length = 0 with
                 |true,true -> 
                                let myString = listTostring theList
                                let myString = myString.[1..]
                                Some myString
                 |false,_ ->
                            let word = string (List.head comfront)
                            let front = List.tail comfront
                            let i = word.Length-2
                            let newWord = word.[..i]

                            let demo i x =
                                let x = string x
                                match x= newWord, x = word with
                                 |true,false -> x+"," , Some (theList.Item (i+1) )
                                 |false,true-> x, Some (theList.Item (i+1) )
                                 |_ -> x, None

                            let tempList = List.mapi demo theList

                            let myFunc x = 
                                match x with
                                 |a,Some b -> (a,b)
                                 |a,_ -> (a,"") 
                            
                            let newMyList,back =
                                let newtempList = List.map myFunc tempList
                                let newTempList = List.rev newtempList

                                let rec separateWords myList (acc1,acc2)  =
                                    match myList with
                                     |[] -> (acc1,acc2)
                                     |head::tail ->
                                             let a,b = head
                                             let b = string b
                                             match b, b.EndsWith(',')|| b.EndsWith('.') with
                                              |"",_ -> separateWords tail (a::acc1,acc2)
                                              |_,true -> let b = b.[..(b.Length-2)]
                                                         separateWords tail (a::acc1,b::acc2)
                                              |_ -> separateWords tail (a::acc1,b::acc2)
                                separateWords newTempList ([],[])

                            sprinkleIt front (back @ comback) newMyList 

                 |_,false -> 
                            let currentword = string (List.head comback)
                            let back = List.tail comback
                            
                            let rec addCommaPrec i x myList front =
                               match x with
                                |[] -> myList,front
                                |h::t ->
                                   match (h = currentword && i-1>=0)|| (currentword+"." = h),(currentword+"," = h)  with
                                    |true,false ->
                                              let addCommma idx list =
                                                let myFunc i (x:string)  =
                                                    match i=idx,x.EndsWith(',')||x.EndsWith('.') with
                                                     |true,false -> x + ",",x
                                                     |_ -> x,""
                                                List.mapi myFunc list
                                              let tempList = (addCommma (i-1) myList)
                                              let tempList = List.rev tempList
                                              let rec separateIt lis (acc1,acc2) =
                                                match lis with
                                                 |[] -> (acc1,acc2)
                                                 |h::t -> 
                                                        let a,b = h
                                                        match b with
                                                         |"" ->  separateIt t (a::acc1,acc2)
                                                         |_ -> separateIt t (a::acc1,b::acc2)
                                              let theTempList,myFront = separateIt tempList ([],[])
                                     
                                              addCommaPrec (i+1) t theTempList (front @ myFront)
                                    |_ -> addCommaPrec (i+1) t myList (front)
                            let myNewList,front = addCommaPrec 0 theList theList []
                            
                            sprinkleIt (front @ comfront) back myNewList

             sprinkleIt front [] myList


let rivers input =
    let input = string input
    let inputlength = input.Length
    let myWords = input.Split()
    let theList = Array.toList(myWords)

    let findmax (state:string) (item:string) = 
        match item.Length>=state.Length with
        |true -> sprintf "%s" item
        |_ -> sprintf "%s" state
    let max = List.fold findmax "" theList
   // let lenmax = max.Length
    let lenmax = 14
    let rec createList width list (acc:string) rezList =
       match list with
         |[] -> acc::rezList
         |h::t ->
                let h = string h
                match (acc.Length + h.Length)>=width with
                 |true -> createList width t h (acc::rezList)
                 |_ -> let mystring = acc + sprintf " %s" h
                       let acc = match mystring.StartsWith(' ') with
                                    |true -> mystring.[1..]
                                    |_ -> mystring
                       createList width t acc rezList
    let myList = createList lenmax theList "" []
    let structuredList = List.rev myList
     
    let rec findit list riverLength width (acc:string) space index shortest =
        match list with
         |[] -> width
         |head::tail ->
                        let hd = string head
                        let myLine = hd.Split()
                        let firstLine = Array.toList(myLine)
                        let myString = string (List.item index firstLine)
                        let spacePosition = myString.Length+acc.Length+space
                        match space  < firstLine.Length-1 with
                         |true -> 
                                let rec checkNext myList total pos =
                                    match myList with
                                     |[] -> total
                                     |h::t -> 
                                             let currentLine = string h
                                             match currentLine.[pos..pos]=" ",currentLine.[pos-1..pos-1]=" ",currentLine.[pos+1..pos+1]=" " with
                                              |true,false,false -> checkNext t (total+1) (pos)
                                              |false,true,false -> checkNext t (total+1) (pos-1)
                                              |false,false,true -> checkNext t (total+1) (pos+1)
                                              |_ -> checkNext [] (total) (0)
                                let rivLen = checkNext tail 1 spacePosition
                                match rivLen>riverLength with
                                 |true -> findit (list) (rivLen) (width) (myString+acc) (space) (index+1) (width)
                                 |_ -> findit list (riverLength) width (myString+acc) space (index+1) width                                
                         |_ -> findit tail riverLength width acc 0 0 shortest

    let nje = findit structuredList 0 lenmax "" 0 0 lenmax
    None
    //failwith "Not implemented"


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code