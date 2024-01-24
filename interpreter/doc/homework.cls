\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{homework}[2021/05/30 Homework]

% Packages
\RequirePackage{xcolor}
\RequirePackage{xparse}
\RequirePackage{graphicx}
\RequirePackage[absolute,overlay]{textpos}
\RequirePackage{amsmath}
\RequirePackage{amssymb}
\RequirePackage{amsthm}
\RequirePackage{stmaryrd}
\RequirePackage{minted}
\RequirePackage{comment}
\RequirePackage{commenters}
% \RequirePackage{../styles/idrislang}

% AMSTHM Setup
\newtheorem{thm}{Theorem}
\newtheorem{lem}{Lemma}
\newtheorem{prop}{Proposition}
\newtheorem{cor}{Corollary}
\theoremstyle{definition}
\newtheorem{defn}{Definition}
\newtheorem*{nb}{Note}
% \newtheorem{prob}{Problem}

% Commenters Setup
\newcommenter{\akh}{purple}{AKH}

% Ignore Hidden Commands
\excludecomment{hidden}

% Macros

%% Title Macros
\newcommand{\coursenumber}[1]{\makeatletter\newcommand{\@courseno}{#1}\makeatother}
\newcommand{\coursename}[1]{\makeatletter\newcommand{\@coursename}{#1}\makeatother}
\newcommand{\hwnumber}[1]{\makeatletter\newcommand{\@hwno}{#1}\makeatother}

%% Problem Macros

\newcounter{prob@no}
\setcounter{prob@no}{0}
\NewDocumentEnvironment{prob}{o}{
  \par\vspace{1em}
  \refstepcounter{prob@no}
  \noindent\textbf{Problem~\theprob@no}\IfValueT{#1}{\noindent\textbf{~(#1)}}\textbf{:}
}
{
  \par
}

\NewDocumentEnvironment{sol}{o}{
  \vspace{1em}
  \par\noindent\IfNoValueTF{#1}{\textbf{Solution:}}{\textbf{#1}} \hspace{0.5em}\par\noindent
  % \leftskip=4em\rightskip=2em
}{
  \par\noindent\hfill$\blacksquare$\par%
  % \leftskip=0em\rightskip=0em
}

% Class Options
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax

\LoadClass[letterpaper]{article}

\renewcommand{\maketitle}{%
  \begin{textblock*}{8in}(0cm,0.25cm)
    \framebox[\textwidth]{
      \vspace{1em}%
      \begin{minipage}{1.0\linewidth}
        \begin{center}
          {\fontsize{15}{20}\fontfamily{phv}\fontseries{b}\selectfont\@courseno:~\@coursename}\\[1em]
          {\fontsize{20}{30}\fontfamily{phv}\fontseries{b}\selectfont Homework~\@hwno:~\@title}%
        \end{center}
      \end{minipage}
      \vspace{1em}%
    }%
  \end{textblock*}%
  \vspace*{1in}%
}

\renewcommand{\section}{\@startsection{section}{1}{-1pt}{-1.5ex plus -1ex minus -0.2ex}{1ex plus .2ex}{\normalfont\Large\bfseries}}

\renewcommand{\subsection}{\@startsection{subsection}{2}{-1pt}{-1.5ex plus -1ex minus -0.2ex}{1ex plus .2ex}{\normalfont\large\bfseries}}

\setlength{\textwidth}{7in}
\setlength{\textheight}{9in}
\topmargin -1in
\oddsidemargin -0.25in
\evensidemargin -0.25in
\setcounter{secnumdepth}{0}
\pagenumbering{arabic}

