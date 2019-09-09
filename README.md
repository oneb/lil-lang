Install on Ubuntu:

    sudo apt-get update
    sudo apt-get install haskell-platform
    sudo apt-get install libreadline-dev
    git clone https://github.com/oneb/lil-lang
    cd lil-lang
    cabal update
    cabal install --only-dependencies  
    cabal configure
    cabal build

Launch the REPL:

    ./dist/build/lil-lang/lil-lang

Use the REPL like this:

    Ready.
    Enter "exit" to exit.
    > 2
    2
    > (+ 2 2)
    4
    > (define x 3)
    3
    > (* 2 x)
    6
    > (define square (lambda (x) (* x x)))
    <a procedure of 1 argument(s)>
    > (square 2)
    4
    > (square x)
    9
    > ((
    could not parse as expression
    > (define compose (lambda (f g) (lambda (x) (f (g x)))))
    <a procedure of 2 argument(s)>
    > ((compose square square) 3)
    81
    > :examples/hello-world.lil
    "Hello, world"
    ok
    > 

