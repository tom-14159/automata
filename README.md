# Automata package for Haskell

In Ubuntu you need these extra packages to compile the module and the application for drawing piecewise-testable languages:

- libghc-missingh-dev,
- libghc-temporary-dev (for the p-t app),
- libghc-readline-dev (libreadline-dev, libncurses5-dev, for the p-t app),
- graphviz (to draw graphs "out of the box").

`$ sudo apt-get install libghc-missingh-dev libghc-readline-dev libghc-temporary-dev graphviz`

## pt

Application for drawing a piece-testable languages over the alphabet {a,b}.

### Syntax

- Language of form A\*.a\_1.A\*.a\_2.A\*...A\*.a\_n.A*: `a\_1a\_2...a\_n` e.g. `aabbab`,
- union: `A|B` e.g. `aaaba|bba`,
- intersection: `A&B` e.g. `(aaaba|bba)&baa`,
- complementation: `-A` e.g. `-((aaaba|bba)&baa)`,
- minimization: `<A>` e.g. `<-((aaaba|bba)&baa)>`.

### Batch mode

`$ ./pt lang` prints automaton of `lang` in graphviz's dot format.

`$ ./pt '<aaba&-bbba>' > graph.dot && dot -Tpdf -o graph.pdf graph.dot`

### Interactive mode

`$ ./pt`

`> <aaba&-bbba>` creates pdf with the automaton using graphviz and shows it using evince.
