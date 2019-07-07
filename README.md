# METAHS
# originally build on:
Haskell Platform 8.0.2
LTS Haskell 13.13 (ghc-8.6.4) which include the following:

Package:                Version:    Remark:
- graphviz              2999.20.0.3 non-lts 13.13
- base >=               4.7 && < 5
- filepath              1.4.2.1
- pretty                1.1.3.6
- directory             1.3.3.0
- containers            0.6.0.1
- bytestring            0.10.8.2
- csv                   0.1.2
- cassava               0.5.1.0
- text                  1.2.3.1
- vector                0.12.0.2

Description:
MetaHs is a library which provides functions to analyse Haskell source code. :q

Usage as GIT submodule in Stack project:
1) Add metaHS library as submodule to you project
2) Add metahs as a executable dependency to your package.yaml file:
executables:
    <program name>:
        <other stuff>
        dependencies:
            metahs
3) Add <submodule location>/metahs to your stack.yaml file:
packages:
- .
- location: module/metahs
4) Also add to your stack.yaml file:
extra-deps:
- graphviz-2999.20.0.3