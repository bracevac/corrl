function help_exit
  echo "Usage: -n|--arity <arity>"
  exit 1
end

argparse "n/arity=!_validate_int --min 1" -- $argv 2> /dev/null; or help_exit
set --query _flag_arity; or set --local _flag_arity $_flag_n
set num $_flag_arity


set -l lst (seq 1 $num)
dune build generator/gen.exe
rm -r benchmarks/
rm -r _build/default/benchmarks/
mkdir -p benchmarks
cp utility.ml count.ml delimcont.ml async.ml handlers.ml hlists.ml data.ml prelude.ml slot.ml yieldfail.ml suspension.ml core2.ml symantics.ml stat.ml dsl2.ml restriction2.ml bench_common.ml benchmarks/
cd ./benchmarks
../_build/default/generator/gen.exe -n $num
mv dune.benchmark dune
for i in perf*ml
    dune build (echo $i | cut -f 1 -d '.').exe --profile release
end
for i in ../_build/default/benchmarks/perf*.exe
  $i
end
