#require "re,re.pcre";;
#directory "_build";;
#load "lexer.cmo";;
let fn = "/usr/local/home/jdimino/workspaces/jane/+share+/.hgignore";;
let re = Lexer.load fn;;
