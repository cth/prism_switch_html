PRISM parameter file visualizer
===============================

This is a simple tool for inspecting and visualizing the parameters of a PRISM. 
When dealing with PRISM programs with many parameters it can be tedious to inspect these using only 
the saved switch file or by interactive reporting, e.g., show_sw.

This tool transforms a PRISM parameters file to a HTML file which shows the parameters arranged in HTML tables. 
The cells of the table are color-coded to provide a visual overview. Relatively high probabilities are colored in 
green and relatively low probabilities are colored in red.

To use it consult the 'sw_to_html.pl' from prism (or b-prolog).

Then to produce an HTML visualization of a parameter file, type

	sw_to_html('examples/example.sw', 'examples/example.html').

License
-------

Copyright (C) 2012 Christian Theil Have 

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
