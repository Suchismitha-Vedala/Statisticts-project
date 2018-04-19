\documentclass[11pt,epsf]{article}
\usepackage{epsfig}
\usepackage{amsmath}
\usepackage{graphicx}
\graphicspath{ {/Users/suchismithavedala/Desktop/Spring 2018/Stat/Project/Data/} }
 

\begin{document}
\title{Project Report}
\maketitle

\section*{Analysis of Scores:}

\textbf{Hypothesis}:\\
$Null Hypothesis : H_0 = $ The mean of scores is same for both the Scorers\\
$Alternate Hypothesis : H_1 = $ The  mean of scores is different for both the Scorers\\
\textbf{Approach:Wilcox Test}:\\
\\
\includegraphics[scale=0.6]{Cutting_Scores}\\
\includegraphics[scale=0.6]{Suturing_Scores}\\
\\
Cutting : When performed Wilcox test, p-value is greater than 0.05, which applies the means have not changed\\
Suturing: When performed Wilcox test, p-value is less than 0.05, which states that the means of the scorers is different. \\
\\
Hence Scorer has an effect for Suturing , not Cutting\\


\end{document}