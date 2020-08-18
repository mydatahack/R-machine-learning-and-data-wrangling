library(shiny)

if(require(UsingR)){
  print('Loading UsingR')
} else {
  install.packages("UsingR")
  if(require(UsingR)){
    print('Loading UsingR')
  } else{
    'Failed to install UsingR'
  }
}

if(require(ggplot2)){
  print('Loading ggplot2')
} else {
  install.packages("ggplot2")
  if(require(ggplot2)){
    print('Loading ggplot2')
  } else{
    'Failed to install ggplot2'
  }
}

if(require(plotly)){
  print('Loading plotly')
} else {
  install.packages('plotly')
  if(require(plotly)){
    print('Loading plotly')
  } else {
    'Failed to install plotly'
  }
}

