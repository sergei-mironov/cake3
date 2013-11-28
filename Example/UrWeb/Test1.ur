fun js {} : transaction xhead =
  return (Script.insert (blessMime "text/javascript") (bless "http://code.jquery.com/ui/1.10.3/jquery-ui.js"))
  

fun main {} : transaction page = 
  s <- js {};
  return
    <xml>
      <head>
        {s}
      </head>
      <body>
      </body>
    </xml>
