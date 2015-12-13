open System
open System.Drawing
open System.Windows.Forms
 
let form = new System.Windows.Forms.Form(Visible=true,Text="Displaying data in F#",TopMost = true, Size=Drawing.Size(600,600))

let textBox = 
    new System.Windows.Forms.RichTextBox(Dock=DockStyle.Fill,
        Text="This is a text box that we can fee data into",
        Font = 
            new System.Drawing.Font("Lucida Console",
                16.0f,
                System.Drawing.FontStyle.Bold),
                ForeColor = System.Drawing.Color.DarkBlue)
let show x = 
    textBox.Text <- sprintf "%30A" x

form.Controls.Add(textBox)
form.Show()
