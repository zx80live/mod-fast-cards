## Linux console flash-cards

####Setup:
1. Assembly project: `> sbt assembly`

####Usage:
1. Create cards file ([see example](https://github.com/zx80live/mod-fast-cards/blob/master/words.txt)):
2. Start program:

`java -jar ./target/scala-2.11/mod-fast-cards.jar words.txt`

or use script: `exam` [script source](https://github.com/zx80live/mod-fast-cards/blob/master/exam):
`exam words.txt`

for help enter command:
`exam --help`

####Arguments:
```
Usage: exam [options] <file>...

  <file>...
        optional unbounded args
  --en-ru
        en-ru mode
  --no-shuffle
        
  -p <value> | --pass-count <value>
        
  -r <value> | --random-words <value>
        
  --no-make-bads
        
  --verbose-parse
        
  -f <type1>,<type2>... | --filter <type1>,<type2>...
        filter cards by type (verb, noun, etc)
```

### Keys:
`s` - show current exam statistic

`i` - show examples for current card

`d` - drop current card

`space/enter` - flip card

`space` - estimate false (for flipped card)

`enter` - estimate true (for flipped card)

`back space` - undo last estimate

`ctrl+D` - exit and create statistic (write *.bad for bad words)
