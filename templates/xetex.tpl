\documentclass[a4paper]{article}
\usepackage{fontspec,xunicode,xltxtra,xeCJK,float,listings,color,fancyhdr,fancybox,comment,latexsym,enumerate}

\setCJKmainfont[BoldFont=YouYuan]{SimSun}
\setmainfont{Courier New}
\setCJKfamilyfont{song}{SimSun}
\setCJKfamilyfont{msyh}{微软雅黑}
\setCJKfamilyfont{fs}{FangSong}

\renewcommand{\baselinestretch}{1.4}
\setlength{\parskip}{5pt}
\setlength{\parindent}{0mm}

% In case you need to adjust margins:
\topmargin=-0.0in      %
\evensidemargin=0.5in     %
\oddsidemargin=0.5in      %
\textwidth=5.5in        %
\textheight=8.5in       %
\headsep=0.25in         %

\renewcommand\thesection{\arabic{section}.\hspace{-4mm}}%自定义section样式

\pagestyle{fancy}
\lhead{\footnotesize }
\chead{\footnotesize }
\rhead{\footnotesize 计92 丘骏鹏 2009011282}
\pagenumbering{Roman}%设置页码编号方式

\lstset{language=C++}
\lstset{breaklines}
\lstset{extendedchars=false, frame=shadowbox, basicstyle=\footnotesize}

\title{\CJKfamily{msyh} }
\author{计92~~丘骏鹏~~2009011282}
\date{}
\begin{document}
\maketitle
\end{document}
