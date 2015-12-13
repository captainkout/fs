let siteRoot = @"C:\MySite\"
let host = "http://localhost:8080/"
 
let listener (handler:(System.Net.HttpListenerRequest->System.Net.HttpListenerResponse->Async<unit>)) =
    let hl = new System.Net.HttpListener()
    hl.Prefixes.Add host
    hl.Start()
    let task = Async.FromBeginEnd(hl.BeginGetContext, hl.EndGetContext)
    async {
        while true do
            let! context = task
            Async.Start(handler context.Request context.Response)
    } |> Async.Start
 
let output (req:System.Net.HttpListenerRequest) =
    let file = System.IO.Path.Combine(siteRoot,
                            System.Uri(host).MakeRelativeUri(req.Url).OriginalString)
    printfn "Requested : '%s'" file
    if (System.IO.File.Exists file)
        then System.IO.File.ReadAllText(file)
        else "File does not exist!"
 
listener (fun req resp ->
    async {
        let txt = System.Text.Encoding.ASCII.GetBytes(output req)
        resp.ContentType <- "text/html"
        resp.OutputStream.Write(txt, 0, txt.Length)
        resp.OutputStream.Close()
    })
//Console.ReadLine() |> ignore
// TODO: add your code here