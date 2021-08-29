question <- function(question, distractors, correct, no, fb = "", print_question = TRUE){
  allanswers <- c(distractors, correct)[sample.int(length(distractors)+1)]
  correctanswer <- which(allanswers == correct)
  answercode <- paste0(sapply(1:length(allanswers), function(i){
    x <- allanswers[i]
    paste0('<div class="radio">\n  <label>\n    <input type="radio" name="question', no, '" id="opt', i,'" value="', i, '" onchange="check_answer', no, '()">\n    ', x, '\n  </label>\n</div>')
  }), collapse = "\n\n")

  out <- paste0(question, "\n\n", answercode,
             '<div class="collapse" id="collapseExample', no,'">
  <div class="card card-body" id="answerFeedback', no, '">
  </div>
</div>',
             paste0('<script type="text/javascript">
function check_answer', no, '()
{
    var radioButtons', no, ' = document.getElementsByName("question', no, '");
    document.getElementById("answerFeedback', no, '").innerHTML = "Try selecting an answer!!";
    for(var i = 0; i < radioButtons', no, '.length; i++)
    {
        if(radioButtons', no, '[i].checked == true)
        {
            var feedback', no, ' = "<p style=\'color:red\'>Wrong', ifelse(fb == "", ".", paste0("; ", fb)),  '</p>";
            if(radioButtons', no, '[i].value == "', correctanswer, '") {
              feedback', no, ' = "<p style=\'color:green\'>Correct!</p>"
            }
            document.getElementById("answerFeedback', no, '").innerHTML = feedback', no, ';
            return true;
        }
    }
}
</script>
'))
  if(print_question){
    cat(out)
  } else {
    return(out)
  }
}


questionnaire <- function(x, shuffle = TRUE, print_question = TRUE){
  if(inherits(x, "character")) x <- read.csv(x, stringsAsFactors = FALSE, fileEncoding="UTF-8-BOM")
  if(!all(names(x) == c("question", "distractors", "correct", "fb"))){
    stop("Incorrect column names")
  }
  if(shuffle){
    x <- x[sample.int(nrow(x)), ]
  }
  out <- ""
  for(i in 1:nrow(x)){
    out <- paste0(out,
                  "**Question ",
                  i,
                  ":**\n",
                  question(question = x$question[i],
                           distractors = eval(parse(text = x$distractors[i])),
                           correct = x$correct[i],
                           no = i,
                           fb = ifelse(is.na(x$fb[i]), "", x$fb[i]),
                           print_question = FALSE),
                  "\n\n"
                  )
  }
  if(print_question){
    cat(out)
  } else {
    return(out)
  }
}
