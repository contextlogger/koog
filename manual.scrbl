#lang scribble/manual
@(require scribble/eval scribble/basic)
@(require (for-label "koog.ss" "cli.ss" "runtime.ss" "util.ss"))

@(require setup/getinfo)
@(define info (get-info (list "koog")))

@title[#:tag "top"]{Koog Manual}

@section[#:tag "concept"]{Concept}

@emph{Mixed-code generation} is a form of template-based code generation where the template file is overwritten with the generated file, with the code generation directives intact. The term was (as far as we know) introduced by Jack Herrington in his book Code Generation in Action (Manning, 2003).

Koog can function as a traditional mixed-code generator for a host language that uses C language style comments. Koog also supports a restricted form of mixed-code generation where only specific regions of the source file are actually regenerated.

@section[#:tag "features"]{Features}

@(itemlist
  @item{Supports PLT Scheme version 4 and Racket (PLT Scheme version 5).}
  @item{Includes a Scheme API, command-line interface, and Emacs/Vim editor integration.}
  (item "It is possible to:"
  (itemlist
    @item{Expand all regions in a file, or only expand a chosen region (specified by   line number).}
    @item{Remove code generation directives (for one-off operations).}
    @item{Simulate an expansion (see what would be changed without changes to the input file).}
    @item{Filter standard input (and print the result to standard output).}
   ))
  )

@section[#:tag "api"]{API}

The provided APIs are fairly self-explanatory, so look at the source code. 

The primary module is @hyperlink["../src/koog.ss"]{@racket[koog/koog]}, which exports the @racket[koog-expand] function, and some parameters affecting its behavior.

The @hyperlink["../src/cli.ss"]{@racket[koog/cli]} module only exports a @racket[main] function that parses command-line options and accordingly sets parameters for @racket[koog-expand] prior to invoking it.

The @hyperlink["../src/runtime.ss"]{@racket[koog/runtime]} module exports parameters that make context information available to code generation directives. It is not necessary for directives to explicitly @racket[require] this module.

@section[#:tag "examples"]{Examples}

Here are some real-world examples from the @hyperlink["http://contextlogger.org/"]{ContextLogger2} codebase:

@(itemlist
  @item{@hyperlink["http://korpi.hiit.fi/contextlogger2/daemon/private-cxx-api/html/epoc-indicator_8hpp-source.html"]{generating code for Symbian two-phase construction};}
  @item{@hyperlink["http://korpi.hiit.fi/contextlogger2/daemon/private-cxx-api/html/lua__bindings_8cpp-source.html"]{generating boilerplate code for Lua bindings};}
  @item{@hyperlink["http://korpi.hiit.fi/contextlogger2/daemon/private-cxx-api/html/cf__rcfile_8cpp-source.html"]{fetching a multi-line string from a file and declaring it as a CPP (preprocessor) definition}.})

@section[#:tag "repo"]{Source Code Repository}

Koog source code is retrievable with @hyperlink["http://git-scm.com/"]{Git}:

@(nested #:style 'inset (tt (string-append "git clone " (info 'repo-url))))

@section[#:tag "faq"]{Hardly Ever Asked Questions (HEAQ)}

@bold{Q:} Where does the name "Koog" come from?@linebreak{}
@bold{A:} It is short for the Finnish word "@emph{koo}di@emph{g}eneraattori" (code generator).

@; Local Variables:
@; mode: longlines
@; End:
