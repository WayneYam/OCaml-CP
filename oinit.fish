function oinit
    if test (echo (pwd) | head -c (string length $OCAML_CP_ROOT)) = $OCAML_CP_ROOT
        set -l filename main
        if test -n "$argv"
            set filename $argv
        end
        cp $OCAML_CP_ROOT/bin/template.ml ./$filename.ml
        cat $OCAML_CP_ROOT/bin/dune | sed -s "s/template/$filename/" >>dune
        nvim $filename.ml
    else
        echo "Error: Not inside OCAML_CP_ROOT"
    end
end
