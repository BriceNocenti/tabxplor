# golden display (tab_md) unchanged: f_row_pct

    Code
      cat(tab_md(cases[[name]](), print = FALSE))
    Output
      <style>
      .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
      .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
      .tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
      .tabxplor-tab tfoot{font-size:80%;text-align:left;}
      .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
      .tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
      .tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
      .tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
      .tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab p{font-size:80%;}
      .tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab .tx-r{text-align:right;}
      .tabxplor-tab .tx-l{text-align:left;}
      .tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
      .tabxplor-tab .tx-num{white-space:nowrap;}
      .tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
      .tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
      .tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
      .tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
      .tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
      .tabxplor-tab .tx-foot{width:0;min-width:100%;}
      .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
      .tooltip-inner{max-width:none;white-space:nowrap;}
      .popover{max-width:none;}
      .popover-body,.popover-content{padding:6px;white-space:nowrap;}
      .tabxplor-tab{color:#000000;background:#ffffff;}
      .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
      .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
      .g1,.tabxplor-tab .g1{color:#9f9f9f;}
      .g2,.tabxplor-tab .g2{color:#111111;}
      .tabxplor-caption{color:#000000;}
      .tabxplor-tab .tx-sec{color:#000000;}
      .p1,.tabxplor-tab .p1{color:#02A5B3;}
      .p2,.tabxplor-tab .p2{color:#0891C9;}
      .p3,.tabxplor-tab .p3{color:#0267C7;}
      .p4,.tabxplor-tab .p4{color:#300DFD;}
      .m1,.tabxplor-tab .m1{color:#DCA331;}
      .m2,.tabxplor-tab .m2{color:#DE7C01;}
      .m3,.tabxplor-tab .m3{color:#DD5301;}
      .m4,.tabxplor-tab .m4{color:#D60103;}
      .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
      .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
      .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
      .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
      .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
      .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
      .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
      .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
      @media print {
        .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
        .tabxplor-tab{color:#000000;background:#ffffff;}
        .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
        .tabxplor-tab tbody tr:hover{background:transparent;}
        .g1,.tabxplor-tab .g1{color:#595959;}
        .g2,.tabxplor-tab .g2{color:#111111;}
        .tabxplor-caption{color:#000000;}
        .tabxplor-tab .tx-sec{color:#000000;}
        .p1,.tabxplor-tab .p1{color:#000000;}
        .p2,.tabxplor-tab .p2{color:#000000;}
        .p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
        .p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline;}
        .m1,.tabxplor-tab .m1{color:#000000;font-weight:normal;font-style:italic;}
        .m2,.tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
        .m3,.tabxplor-tab .m3{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .m4,.tabxplor-tab .m4{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
        .o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
        .o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
        .o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
        .u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
        .u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
        .u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
        .u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
      }
      </style>
      
      ::: {.tabxplor-tab}
      | marital       | |Other  |Black  |White  | |          Total  |
      |:--------------|-|------:|------:|------:|-|----------------:|
      |               | | *race*|       |       | |                 |
      |               | |       |       |       | |                 |
      | No answer     | |  12%  |  12%  |  76%  | |  100% (    17)  |
      | Never married | |  12%  |  24%  |  64%  | |  100% ( 5 416)  |
      | Separated     | |  15%  |  26%  |  59%  | |  100% (   743)  |
      | Divorced      | |   6%  |  15%  |  79%  | |  100% ( 3 383)  |
      | Widowed       | |   4%  |  14%  |  82%  | |  100% ( 1 807)  |
      | Married       | |   9%  |   9%  |  82%  | |  100% (10 117)  |
      |**Total**      | | **9%**|**15%**|**76%**| |**100%** (21 483)|
      :::

# golden display (tab_md) unchanged: f_ci_cell

    Code
      cat(tab_md(cases[[name]](), print = FALSE))
    Output
      <style>
      .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
      .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
      .tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
      .tabxplor-tab tfoot{font-size:80%;text-align:left;}
      .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
      .tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
      .tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
      .tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
      .tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab p{font-size:80%;}
      .tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab .tx-r{text-align:right;}
      .tabxplor-tab .tx-l{text-align:left;}
      .tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
      .tabxplor-tab .tx-num{white-space:nowrap;}
      .tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
      .tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
      .tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
      .tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
      .tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
      .tabxplor-tab .tx-foot{width:0;min-width:100%;}
      .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
      .tooltip-inner{max-width:none;white-space:nowrap;}
      .popover{max-width:none;}
      .popover-body,.popover-content{padding:6px;white-space:nowrap;}
      .tabxplor-tab{color:#000000;background:#ffffff;}
      .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
      .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
      .g1,.tabxplor-tab .g1{color:#9f9f9f;}
      .g2,.tabxplor-tab .g2{color:#111111;}
      .tabxplor-caption{color:#000000;}
      .tabxplor-tab .tx-sec{color:#000000;}
      .p1,.tabxplor-tab .p1{color:#02A5B3;}
      .p2,.tabxplor-tab .p2{color:#0891C9;}
      .p3,.tabxplor-tab .p3{color:#0267C7;}
      .p4,.tabxplor-tab .p4{color:#300DFD;}
      .m1,.tabxplor-tab .m1{color:#DCA331;}
      .m2,.tabxplor-tab .m2{color:#DE7C01;}
      .m3,.tabxplor-tab .m3{color:#DD5301;}
      .m4,.tabxplor-tab .m4{color:#D60103;}
      .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
      .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
      .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
      .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
      .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
      .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
      .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
      .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
      @media print {
        .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
        .tabxplor-tab{color:#000000;background:#ffffff;}
        .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
        .tabxplor-tab tbody tr:hover{background:transparent;}
        .g1,.tabxplor-tab .g1{color:#595959;}
        .g2,.tabxplor-tab .g2{color:#111111;}
        .tabxplor-caption{color:#000000;}
        .tabxplor-tab .tx-sec{color:#000000;}
        .p1,.tabxplor-tab .p1{color:#000000;}
        .p2,.tabxplor-tab .p2{color:#000000;}
        .p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
        .p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline;}
        .m1,.tabxplor-tab .m1{color:#000000;font-weight:normal;font-style:italic;}
        .m2,.tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
        .m3,.tabxplor-tab .m3{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .m4,.tabxplor-tab .m4{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
        .o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
        .o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
        .o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
        .u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
        .u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
        .u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
        .u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
      }
      </style>
      
      ::: {.tabxplor-tab}
      | marital       | |    Other  |     Black  |     White  | |          Total  |
      |:--------------|-|----------:|-----------:|-----------:|-|----------------:|
      |               | | *race*    |            |            | |                 |
      |               | |           |            |            | |                 |
      | No answer     | |  [3;34]%  |   [3;34]%  |  [53;90]%  | |  100% (    17)  |
      | Never married | | [11;13]%  |  [23;25]%  |  [63;65]%  | |  100% ( 5 416)  |
      | Separated     | | [12;18]%  |  [23;30]%  |  [55;62]%  | |  100% (   743)  |
      | Divorced      | |   [5;7]%  |  [13;16]%  |  [78;80]%  | |  100% ( 3 383)  |
      | Widowed       | |   [3;5]%  |  [13;16]%  |  [80;83]%  | |  100% ( 1 807)  |
      | Married       | |  [9;10]%  |    [8;9]%  |  [81;83]%  | |  100% (10 117)  |
      |**Total**      | |**[9;10]%**|**[14;15]%**|**[76;77]%**| |**100%** (21 483)|
      :::

# golden display (tab_md) unchanged: f_ci_diff

    Code
      cat(tab_md(cases[[name]](), print = FALSE))
    Output
      <style>
      .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
      .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
      .tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
      .tabxplor-tab tfoot{font-size:80%;text-align:left;}
      .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
      .tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
      .tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
      .tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
      .tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab p{font-size:80%;}
      .tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab .tx-r{text-align:right;}
      .tabxplor-tab .tx-l{text-align:left;}
      .tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
      .tabxplor-tab .tx-num{white-space:nowrap;}
      .tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
      .tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
      .tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
      .tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
      .tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
      .tabxplor-tab .tx-foot{width:0;min-width:100%;}
      .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
      .tooltip-inner{max-width:none;white-space:nowrap;}
      .popover{max-width:none;}
      .popover-body,.popover-content{padding:6px;white-space:nowrap;}
      .tabxplor-tab{color:#000000;background:#ffffff;}
      .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
      .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
      .g1,.tabxplor-tab .g1{color:#9f9f9f;}
      .g2,.tabxplor-tab .g2{color:#111111;}
      .tabxplor-caption{color:#000000;}
      .tabxplor-tab .tx-sec{color:#000000;}
      .p1,.tabxplor-tab .p1{color:#02A5B3;}
      .p2,.tabxplor-tab .p2{color:#0891C9;}
      .p3,.tabxplor-tab .p3{color:#0267C7;}
      .p4,.tabxplor-tab .p4{color:#300DFD;}
      .m1,.tabxplor-tab .m1{color:#DCA331;}
      .m2,.tabxplor-tab .m2{color:#DE7C01;}
      .m3,.tabxplor-tab .m3{color:#DD5301;}
      .m4,.tabxplor-tab .m4{color:#D60103;}
      .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
      .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
      .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
      .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
      .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
      .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
      .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
      .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
      @media print {
        .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
        .tabxplor-tab{color:#000000;background:#ffffff;}
        .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
        .tabxplor-tab tbody tr:hover{background:transparent;}
        .g1,.tabxplor-tab .g1{color:#595959;}
        .g2,.tabxplor-tab .g2{color:#111111;}
        .tabxplor-caption{color:#000000;}
        .tabxplor-tab .tx-sec{color:#000000;}
        .p1,.tabxplor-tab .p1{color:#000000;}
        .p2,.tabxplor-tab .p2{color:#000000;}
        .p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
        .p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline;}
        .m1,.tabxplor-tab .m1{color:#000000;font-weight:normal;font-style:italic;}
        .m2,.tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
        .m3,.tabxplor-tab .m3{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .m4,.tabxplor-tab .m4{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
        .o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
        .o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
        .o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
        .u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
        .u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
        .u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
        .u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
      }
      </style>
      
      ::: {.tabxplor-tab}
      | marital       | |  Other  |   Black  |   White  | |          Total  |
      |:--------------|-|--------:|---------:|---------:|-|----------------:|
      |               | | *race*  |          |          | |                 |
      |               | |         |          |          | |                 |
      | No answer     | | 12%     |  12%     |  76%     | |  100% (    17)  |
      | Never married | | 12%***  |  24%***  |  64%***  | |  100% ( 5 416)  |
      | Separated     | | 15%***  |  26%***  |  59%***  | |  100% (   743)  |
      | Divorced      | |  6%***  |  15%     |  79%***  | |  100% ( 3 383)  |
      | Widowed       | |  4%***  |  14%     |  82%***  | |  100% ( 1 807)  |
      | Married       | |  9%     |   9%***  |  82%***  | |  100% (10 117)  |
      |**Total**      | |**9%**   |**15%**   |**76%**   | |**100%** (21 483)|
      
      \*\*\*: significantly different from the reference category (in bold) at the 99% confidence level; \*\*: at the 95% level; \*: at the 90% level; no star: not significant.
      :::

# golden display (tab_md) unchanged: f_color_diff

    Code
      cat(tab_md(cases[[name]](), print = FALSE))
    Output
      <style>
      .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
      .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
      .tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
      .tabxplor-tab tfoot{font-size:80%;text-align:left;}
      .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
      .tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
      .tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
      .tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
      .tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab p{font-size:80%;}
      .tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab .tx-r{text-align:right;}
      .tabxplor-tab .tx-l{text-align:left;}
      .tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
      .tabxplor-tab .tx-num{white-space:nowrap;}
      .tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
      .tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
      .tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
      .tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
      .tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
      .tabxplor-tab .tx-foot{width:0;min-width:100%;}
      .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
      .tooltip-inner{max-width:none;white-space:nowrap;}
      .popover{max-width:none;}
      .popover-body,.popover-content{padding:6px;white-space:nowrap;}
      .tabxplor-tab{color:#000000;background:#ffffff;}
      .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
      .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
      .g1,.tabxplor-tab .g1{color:#9f9f9f;}
      .g2,.tabxplor-tab .g2{color:#111111;}
      .tabxplor-caption{color:#000000;}
      .tabxplor-tab .tx-sec{color:#000000;}
      .p1,.tabxplor-tab .p1{color:#02A5B3;}
      .p2,.tabxplor-tab .p2{color:#0891C9;}
      .p3,.tabxplor-tab .p3{color:#0267C7;}
      .p4,.tabxplor-tab .p4{color:#300DFD;}
      .m1,.tabxplor-tab .m1{color:#DCA331;}
      .m2,.tabxplor-tab .m2{color:#DE7C01;}
      .m3,.tabxplor-tab .m3{color:#DD5301;}
      .m4,.tabxplor-tab .m4{color:#D60103;}
      .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
      .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
      .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
      .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
      .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
      .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
      .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
      .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
      @media print {
        .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
        .tabxplor-tab{color:#000000;background:#ffffff;}
        .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
        .tabxplor-tab tbody tr:hover{background:transparent;}
        .g1,.tabxplor-tab .g1{color:#595959;}
        .g2,.tabxplor-tab .g2{color:#111111;}
        .tabxplor-caption{color:#000000;}
        .tabxplor-tab .tx-sec{color:#000000;}
        .p1,.tabxplor-tab .p1{color:#000000;}
        .p2,.tabxplor-tab .p2{color:#000000;}
        .p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
        .p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline;}
        .m1,.tabxplor-tab .m1{color:#000000;font-weight:normal;font-style:italic;}
        .m2,.tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
        .m3,.tabxplor-tab .m3{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .m4,.tabxplor-tab .m4{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
        .o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
        .o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
        .o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
        .u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
        .u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
        .u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
        .u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
      }
      </style>
      
      ::: {.tabxplor-tab}
      | marital       | |      Other  |       Black  |       White  | |              Total  |
      |:--------------|-|------------:|-------------:|-------------:|-|--------------------:|
      |               | | *race*      |              |              | |                     |
      |               | |             |              |              | |                     |
      | No answer     | |   12%       |    12%       |    76%       | |      100% (    17)  |
      | Never married | |   12%       |   [24%]{.p1} |   [64%]{.m2} | |      100% ( 5 416)  |
      | Separated     | |  [15%]{.p1} |   [26%]{.p2} |   [59%]{.m2} | |      100% (   743)  |
      | Divorced      | |    6%       |    15%       |    79%       | |      100% ( 3 383)  |
      | Widowed       | |   [4%]{.m1} |    14%       |   [82%]{.p1} | |      100% ( 1 807)  |
      | Married       | |    9%       |    [9%]{.m1} |   [82%]{.p1} | |      100% (10 117)  |
      |**Total**      | |  **9%**     |  **15%**     |  **76%**     | |  **100%** (21 483)  |
      
      Shades of blue: cells ≥ the Total row **[+5]{.p1}**; **[+10]{.p2}**; **[+20]{.p3}**; **[+30]{.p4}** points. Shades of yellow to red: cells ≤ the Total row **[-5]{.m1}**; **[-10]{.m2}**; **[-20]{.m3}**; **[-30]{.m4}** points.
      :::

# golden display (tab_md) unchanged: n_mean

    Code
      cat(tab_md(cases[[name]](), print = FALSE))
    Output
      <style>
      .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
      .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
      .tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
      .tabxplor-tab tfoot{font-size:80%;text-align:left;}
      .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
      .tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
      .tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
      .tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
      .tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab p{font-size:80%;}
      .tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab .tx-r{text-align:right;}
      .tabxplor-tab .tx-l{text-align:left;}
      .tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
      .tabxplor-tab .tx-num{white-space:nowrap;}
      .tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
      .tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
      .tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
      .tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
      .tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
      .tabxplor-tab .tx-foot{width:0;min-width:100%;}
      .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
      .tooltip-inner{max-width:none;white-space:nowrap;}
      .popover{max-width:none;}
      .popover-body,.popover-content{padding:6px;white-space:nowrap;}
      .tabxplor-tab{color:#000000;background:#ffffff;}
      .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
      .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
      .g1,.tabxplor-tab .g1{color:#9f9f9f;}
      .g2,.tabxplor-tab .g2{color:#111111;}
      .tabxplor-caption{color:#000000;}
      .tabxplor-tab .tx-sec{color:#000000;}
      .p1,.tabxplor-tab .p1{color:#02A5B3;}
      .p2,.tabxplor-tab .p2{color:#0891C9;}
      .p3,.tabxplor-tab .p3{color:#0267C7;}
      .p4,.tabxplor-tab .p4{color:#300DFD;}
      .m1,.tabxplor-tab .m1{color:#DCA331;}
      .m2,.tabxplor-tab .m2{color:#DE7C01;}
      .m3,.tabxplor-tab .m3{color:#DD5301;}
      .m4,.tabxplor-tab .m4{color:#D60103;}
      .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
      .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
      .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
      .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
      .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
      .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
      .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
      .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
      @media print {
        .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
        .tabxplor-tab{color:#000000;background:#ffffff;}
        .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
        .tabxplor-tab tbody tr:hover{background:transparent;}
        .g1,.tabxplor-tab .g1{color:#595959;}
        .g2,.tabxplor-tab .g2{color:#111111;}
        .tabxplor-caption{color:#000000;}
        .tabxplor-tab .tx-sec{color:#000000;}
        .p1,.tabxplor-tab .p1{color:#000000;}
        .p2,.tabxplor-tab .p2{color:#000000;}
        .p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
        .p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline;}
        .m1,.tabxplor-tab .m1{color:#000000;font-weight:normal;font-style:italic;}
        .m2,.tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
        .m3,.tabxplor-tab .m3{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .m4,.tabxplor-tab .m4{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
        .o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
        .o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
        .o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
        .u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
        .u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
        .u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
        .u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
      }
      </style>
      
      ::: {.tabxplor-tab}
      |               | race                | |              mean (sd)  | |            mean (sd)  |
      |:--------------|:--------------------|-|------------------------:|-|----------------------:|
      |               |                     | | *age*                   | | *tvhours*             |
      |               |                     | |                         | |                       |
      | No answer     | Other               | |     [34.0 (σ8.5 )]{.m3} | |     [2.0       ]{.m1} |
      |               | Black               | |     [64.0        ]{.p4} | |                       |
      |               | White               | |     [56.0 (σ15.7)]{.p3} | |      2.6 (σ1.2)       |
      |               | Total No answer     | |     [52.4 (σ16.5)]{.p1} | |      2.6 (σ1.1)       |
      |               |                     | |                         | |                       |
      | Never married | Other               | |     [30.2 (σ10.6)]{.m4} | |      2.8 (σ2.7)       |
      |               | Black               | |     [34.5 (σ12.1)]{.m3} | |     [4.2 (σ3.4)]{.p1} |
      |               | White               | |     [34.4 (σ14.3)]{.m3} | |      2.8 (σ2.6)       |
      |               | Total Never married | |     [33.9 (σ13.5)]{.m3} | |      3.1 (σ2.9)       |
      |               |                     | |                         | |                       |
      | Separated     | Other               | |     [42.5 (σ13.0)]{.m1} | |      3.3 (σ3.3)       |
      |               | Black               | |      46.2 (σ13.4)       | |     [5.1 (σ4.7)]{.p4} |
      |               | White               | |      45.6 (σ13.5)       | |      2.9 (σ2.8)       |
      |               | Total Separated     | |      45.3 (σ13.4)       | |     [3.5 (σ3.6)]{.p1} |
      |               |                     | |                         | |                       |
      | Divorced      | Other               | |      45.5 (σ11.8)       | |      3.0 (σ2.7)       |
      |               | Black               | |     [51.0 (σ12.7)]{.p1} | |     [4.3 (σ3.7)]{.p3} |
      |               | White               | |     [51.6 (σ13.2)]{.p1} | |      2.9 (σ2.4)       |
      |               | Total Divorced      | |     [51.1 (σ13.1)]{.p1} | |      3.1 (σ2.7)       |
      |               |                     | |                         | |                       |
      | Widowed       | Other               | |     [64.5 (σ14.8)]{.p4} | |     [4.2 (σ2.8)]{.p1} |
      |               | Black               | |     [67.5 (σ13.9)]{.p4} | |     [4.7 (σ3.7)]{.p3} |
      |               | White               | |     [72.8 (σ12.5)]{.p4} | |     [3.7 (σ2.7)]{.p1} |
      |               | Total Widowed       | |     [71.7 (σ13.0)]{.p4} | |     [3.9 (σ2.9)]{.p1} |
      |               |                     | |                         | |                       |
      | Married       | Other               | |     [42.2 (σ13.0)]{.m1} | |      2.5 (σ1.9)       |
      |               | Black               | |      46.4 (σ13.4)       | |     [3.8 (σ3.1)]{.p1} |
      |               | White               | |      49.7 (σ15.2)       | |      2.6 (σ2.0)       |
      |               | Total Married       | |      48.7 (σ15.1)       | |      2.7 (σ2.1)       |
      |               |                     | |                         | |                       |
      | Ensemble      |**Total Ensemble**   | |  **47.2** (σ17.3)       | |  **3.0** (σ2.6)       |
      
      Shades of blue: cells ≥ the Total row **[+0.2]{.p1}**; **[+0.5]{.p3}**; **[+0.8]{.p4}** SD. Shades of yellow to red: cells ≤ the Total row **[-0.2]{.m1}**; **[-0.5]{.m3}**; **[-0.8]{.m4}** SD.
      :::

# golden display (tab_md) unchanged: n_mean_ci

    Code
      cat(tab_md(cases[[name]](), print = FALSE))
    Output
      <style>
      .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
      .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
      .tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
      .tabxplor-tab tfoot{font-size:80%;text-align:left;}
      .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
      .tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
      .tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
      .tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
      .tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab p{font-size:80%;}
      .tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab .tx-r{text-align:right;}
      .tabxplor-tab .tx-l{text-align:left;}
      .tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
      .tabxplor-tab .tx-num{white-space:nowrap;}
      .tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
      .tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
      .tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
      .tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
      .tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
      .tabxplor-tab .tx-foot{width:0;min-width:100%;}
      .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
      .tooltip-inner{max-width:none;white-space:nowrap;}
      .popover{max-width:none;}
      .popover-body,.popover-content{padding:6px;white-space:nowrap;}
      .tabxplor-tab{color:#000000;background:#ffffff;}
      .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
      .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
      .g1,.tabxplor-tab .g1{color:#9f9f9f;}
      .g2,.tabxplor-tab .g2{color:#111111;}
      .tabxplor-caption{color:#000000;}
      .tabxplor-tab .tx-sec{color:#000000;}
      .p1,.tabxplor-tab .p1{color:#02A5B3;}
      .p2,.tabxplor-tab .p2{color:#0891C9;}
      .p3,.tabxplor-tab .p3{color:#0267C7;}
      .p4,.tabxplor-tab .p4{color:#300DFD;}
      .m1,.tabxplor-tab .m1{color:#DCA331;}
      .m2,.tabxplor-tab .m2{color:#DE7C01;}
      .m3,.tabxplor-tab .m3{color:#DD5301;}
      .m4,.tabxplor-tab .m4{color:#D60103;}
      .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
      .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
      .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
      .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
      .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
      .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
      .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
      .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
      @media print {
        .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
        .tabxplor-tab{color:#000000;background:#ffffff;}
        .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
        .tabxplor-tab tbody tr:hover{background:transparent;}
        .g1,.tabxplor-tab .g1{color:#595959;}
        .g2,.tabxplor-tab .g2{color:#111111;}
        .tabxplor-caption{color:#000000;}
        .tabxplor-tab .tx-sec{color:#000000;}
        .p1,.tabxplor-tab .p1{color:#000000;}
        .p2,.tabxplor-tab .p2{color:#000000;}
        .p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
        .p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline;}
        .m1,.tabxplor-tab .m1{color:#000000;font-weight:normal;font-style:italic;}
        .m2,.tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
        .m3,.tabxplor-tab .m3{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .m4,.tabxplor-tab .m4{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
        .o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
        .o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
        .o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
        .u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
        .u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
        .u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
        .u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
      }
      </style>
      
      ::: {.tabxplor-tab}
      | race  | |        mean  | |      mean  |
      |:------|-|-------------:|-|-----------:|
      |       | | *age*        | | *tvhours*  |
      |       | |              | |            |
      | Other | | [38.8;40.1]  | | [2.6;2.9]  |
      | Black | | [43.3;44.5]  | | [4.0;4.3]  |
      | White | | [48.5;49.0]  | | [2.7;2.8]  |
      :::

# golden display (tab_md) unchanged: totn_drop

    Code
      cat(tab_md(cases[[name]](), print = FALSE))
    Output
      <style>
      .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
      .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
      .tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
      .tabxplor-tab tfoot{font-size:80%;text-align:left;}
      .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
      .tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
      .tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
      .tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
      .tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab p{font-size:80%;}
      .tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab .tx-r{text-align:right;}
      .tabxplor-tab .tx-l{text-align:left;}
      .tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
      .tabxplor-tab .tx-num{white-space:nowrap;}
      .tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
      .tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
      .tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
      .tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
      .tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
      .tabxplor-tab .tx-foot{width:0;min-width:100%;}
      .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
      .tooltip-inner{max-width:none;white-space:nowrap;}
      .popover{max-width:none;}
      .popover-body,.popover-content{padding:6px;white-space:nowrap;}
      .tabxplor-tab{color:#000000;background:#ffffff;}
      .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
      .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
      .g1,.tabxplor-tab .g1{color:#9f9f9f;}
      .g2,.tabxplor-tab .g2{color:#111111;}
      .tabxplor-caption{color:#000000;}
      .tabxplor-tab .tx-sec{color:#000000;}
      .p1,.tabxplor-tab .p1{color:#02A5B3;}
      .p2,.tabxplor-tab .p2{color:#0891C9;}
      .p3,.tabxplor-tab .p3{color:#0267C7;}
      .p4,.tabxplor-tab .p4{color:#300DFD;}
      .m1,.tabxplor-tab .m1{color:#DCA331;}
      .m2,.tabxplor-tab .m2{color:#DE7C01;}
      .m3,.tabxplor-tab .m3{color:#DD5301;}
      .m4,.tabxplor-tab .m4{color:#D60103;}
      .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
      .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
      .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
      .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
      .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
      .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
      .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
      .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
      @media print {
        .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
        .tabxplor-tab{color:#000000;background:#ffffff;}
        .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
        .tabxplor-tab tbody tr:hover{background:transparent;}
        .g1,.tabxplor-tab .g1{color:#595959;}
        .g2,.tabxplor-tab .g2{color:#111111;}
        .tabxplor-caption{color:#000000;}
        .tabxplor-tab .tx-sec{color:#000000;}
        .p1,.tabxplor-tab .p1{color:#000000;}
        .p2,.tabxplor-tab .p2{color:#000000;}
        .p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
        .p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline;}
        .m1,.tabxplor-tab .m1{color:#000000;font-weight:normal;font-style:italic;}
        .m2,.tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
        .m3,.tabxplor-tab .m3{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .m4,.tabxplor-tab .m4{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
        .o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
        .o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
        .o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
        .u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
        .u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
        .u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
        .u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
      }
      </style>
      
      ::: {.tabxplor-tab}
      | g       | |     x  |     y  | |     p  |     q  |     r  | | Total  |
      |:--------|-|-------:|-------:|-|-------:|-------:|-------:|-|-------:|
      |         | | *h*    |        | | *k*    |        |        | |        |
      |         | |        |        | |        |        |        | |        |
      | A       | |   35%  |   29%  | |   30%  |   37%  |   36%  | |   34%  |
      | B       | |   36%  |   38%  | |   36%  |   37%  |   35%  | |   36%  |
      | C       | |   29%  |   33%  | |   34%  |   26%  |   29%  | |   30%  |
      |**Total**| |**100%**|**100%**| |**100%**|**100%**|**100%**| |**100%**|
      | n       | |   244  |   270  | |   183  |   169  |   193  | |   545  |
      :::

# golden display (tab_md) unchanged: f_selfcross

    Code
      cat(tab_md(cases[[name]](), print = FALSE))
    Output
      <style>
      .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
      .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
      .tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
      .tabxplor-tab tfoot{font-size:80%;text-align:left;}
      .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
      .tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
      .tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
      .tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
      .tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab p{font-size:80%;}
      .tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab .tx-r{text-align:right;}
      .tabxplor-tab .tx-l{text-align:left;}
      .tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
      .tabxplor-tab .tx-num{white-space:nowrap;}
      .tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
      .tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
      .tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
      .tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
      .tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
      .tabxplor-tab .tx-foot{width:0;min-width:100%;}
      .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
      .tooltip-inner{max-width:none;white-space:nowrap;}
      .popover{max-width:none;}
      .popover-body,.popover-content{padding:6px;white-space:nowrap;}
      .tabxplor-tab{color:#000000;background:#ffffff;}
      .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
      .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
      .g1,.tabxplor-tab .g1{color:#9f9f9f;}
      .g2,.tabxplor-tab .g2{color:#111111;}
      .tabxplor-caption{color:#000000;}
      .tabxplor-tab .tx-sec{color:#000000;}
      .p1,.tabxplor-tab .p1{color:#02A5B3;}
      .p2,.tabxplor-tab .p2{color:#0891C9;}
      .p3,.tabxplor-tab .p3{color:#0267C7;}
      .p4,.tabxplor-tab .p4{color:#300DFD;}
      .m1,.tabxplor-tab .m1{color:#DCA331;}
      .m2,.tabxplor-tab .m2{color:#DE7C01;}
      .m3,.tabxplor-tab .m3{color:#DD5301;}
      .m4,.tabxplor-tab .m4{color:#D60103;}
      .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
      .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
      .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
      .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
      .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
      .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
      .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
      .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
      @media print {
        .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
        .tabxplor-tab{color:#000000;background:#ffffff;}
        .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
        .tabxplor-tab tbody tr:hover{background:transparent;}
        .g1,.tabxplor-tab .g1{color:#595959;}
        .g2,.tabxplor-tab .g2{color:#111111;}
        .tabxplor-caption{color:#000000;}
        .tabxplor-tab .tx-sec{color:#000000;}
        .p1,.tabxplor-tab .p1{color:#000000;}
        .p2,.tabxplor-tab .p2{color:#000000;}
        .p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
        .p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline;}
        .m1,.tabxplor-tab .m1{color:#000000;font-weight:normal;font-style:italic;}
        .m2,.tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
        .m3,.tabxplor-tab .m3{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .m4,.tabxplor-tab .m4{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
        .o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
        .o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
        .o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
        .u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
        .u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
        .u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
        .u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
      }
      </style>
      
      ::: {.tabxplor-tab}
      | marital       | |No answer  |Never married  |Separated  |Divorced  |Widowed  |Married  | |          Total  |
      |:--------------|-|----------:|--------------:|----------:|---------:|--------:|--------:|-|----------------:|
      |               | | *marital* |               |           |          |         |         | |                 |
      |               | |           |               |           |          |         |         | |                 |
      | No answer     | |     100%  |           0%  |       0%  |      0%  |     0%  |     0%  | |  100% (    17)  |
      | Never married | |       0%  |         100%  |       0%  |      0%  |     0%  |     0%  | |  100% ( 5 416)  |
      | Separated     | |       0%  |           0%  |     100%  |      0%  |     0%  |     0%  | |  100% (   743)  |
      | Divorced      | |       0%  |           0%  |       0%  |    100%  |     0%  |     0%  | |  100% ( 3 383)  |
      | Widowed       | |       0%  |           0%  |       0%  |      0%  |   100%  |     0%  | |  100% ( 1 807)  |
      | Married       | |       0%  |           0%  |       0%  |      0%  |     0%  |   100%  | |  100% (10 117)  |
      |**Total**      | |     **0%**|        **25%**|     **3%**|   **16%**|   **8%**|  **47%**| |**100%** (21 483)|
      :::

# golden display (tab_md) unchanged: n_mean_w

    Code
      cat(tab_md(cases[[name]](), print = FALSE))
    Output
      <style>
      .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
      .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
      .tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
      .tabxplor-tab tfoot{font-size:80%;text-align:left;}
      .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
      .tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
      .tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
      .tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
      .tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab p{font-size:80%;}
      .tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab .tx-r{text-align:right;}
      .tabxplor-tab .tx-l{text-align:left;}
      .tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
      .tabxplor-tab .tx-num{white-space:nowrap;}
      .tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
      .tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
      .tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
      .tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
      .tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
      .tabxplor-tab .tx-foot{width:0;min-width:100%;}
      .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
      .tooltip-inner{max-width:none;white-space:nowrap;}
      .popover{max-width:none;}
      .popover-body,.popover-content{padding:6px;white-space:nowrap;}
      .tabxplor-tab{color:#000000;background:#ffffff;}
      .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
      .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
      .g1,.tabxplor-tab .g1{color:#9f9f9f;}
      .g2,.tabxplor-tab .g2{color:#111111;}
      .tabxplor-caption{color:#000000;}
      .tabxplor-tab .tx-sec{color:#000000;}
      .p1,.tabxplor-tab .p1{color:#02A5B3;}
      .p2,.tabxplor-tab .p2{color:#0891C9;}
      .p3,.tabxplor-tab .p3{color:#0267C7;}
      .p4,.tabxplor-tab .p4{color:#300DFD;}
      .m1,.tabxplor-tab .m1{color:#DCA331;}
      .m2,.tabxplor-tab .m2{color:#DE7C01;}
      .m3,.tabxplor-tab .m3{color:#DD5301;}
      .m4,.tabxplor-tab .m4{color:#D60103;}
      .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
      .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
      .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
      .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
      .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
      .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
      .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
      .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
      @media print {
        .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
        .tabxplor-tab{color:#000000;background:#ffffff;}
        .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
        .tabxplor-tab tbody tr:hover{background:transparent;}
        .g1,.tabxplor-tab .g1{color:#595959;}
        .g2,.tabxplor-tab .g2{color:#111111;}
        .tabxplor-caption{color:#000000;}
        .tabxplor-tab .tx-sec{color:#000000;}
        .p1,.tabxplor-tab .p1{color:#000000;}
        .p2,.tabxplor-tab .p2{color:#000000;}
        .p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
        .p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline;}
        .m1,.tabxplor-tab .m1{color:#000000;font-weight:normal;font-style:italic;}
        .m2,.tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
        .m3,.tabxplor-tab .m3{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .m4,.tabxplor-tab .m4{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
        .o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
        .o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
        .o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
        .u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
        .u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
        .u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
        .u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
      }
      </style>
      
      ::: {.tabxplor-tab}
      | g       | |    mean (sd)  |
      |:--------|-|--------------:|
      |         | | *v*           |
      |         | |               |
      | A       | |      10 (σ3)  |
      | B       | |      10 (σ3)  |
      | C       | |      10 (σ3)  |
      |**Total**| |  **10** (σ3)  |
      
      Weighted by w; confidence intervals and tests use the unweighted sample size.
      Shades of blue: cells ≥ the Total row **[+0.2]{.p1}**; **[+0.5]{.p3}**; **[+0.8]{.p4}** SD. Shades of yellow to red: cells ≤ the Total row **[-0.2]{.m1}**; **[-0.5]{.m3}**; **[-0.8]{.m4}** SD.
      :::

# golden display (tab_md) unchanged: n_mean_sparse

    Code
      cat(tab_md(cases[[name]](), print = FALSE))
    Output
      <style>
      .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
      .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
      .tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
      .tabxplor-tab tfoot{font-size:80%;text-align:left;}
      .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
      .tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
      .tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
      .tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
      .tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab p{font-size:80%;}
      .tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab .tx-r{text-align:right;}
      .tabxplor-tab .tx-l{text-align:left;}
      .tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
      .tabxplor-tab .tx-num{white-space:nowrap;}
      .tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
      .tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
      .tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
      .tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
      .tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
      .tabxplor-tab .tx-foot{width:0;min-width:100%;}
      .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
      .tooltip-inner{max-width:none;white-space:nowrap;}
      .popover{max-width:none;}
      .popover-body,.popover-content{padding:6px;white-space:nowrap;}
      .tabxplor-tab{color:#000000;background:#ffffff;}
      .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
      .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
      .g1,.tabxplor-tab .g1{color:#9f9f9f;}
      .g2,.tabxplor-tab .g2{color:#111111;}
      .tabxplor-caption{color:#000000;}
      .tabxplor-tab .tx-sec{color:#000000;}
      .p1,.tabxplor-tab .p1{color:#02A5B3;}
      .p2,.tabxplor-tab .p2{color:#0891C9;}
      .p3,.tabxplor-tab .p3{color:#0267C7;}
      .p4,.tabxplor-tab .p4{color:#300DFD;}
      .m1,.tabxplor-tab .m1{color:#DCA331;}
      .m2,.tabxplor-tab .m2{color:#DE7C01;}
      .m3,.tabxplor-tab .m3{color:#DD5301;}
      .m4,.tabxplor-tab .m4{color:#D60103;}
      .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
      .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
      .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
      .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
      .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
      .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
      .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
      .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
      @media print {
        .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
        .tabxplor-tab{color:#000000;background:#ffffff;}
        .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
        .tabxplor-tab tbody tr:hover{background:transparent;}
        .g1,.tabxplor-tab .g1{color:#595959;}
        .g2,.tabxplor-tab .g2{color:#111111;}
        .tabxplor-caption{color:#000000;}
        .tabxplor-tab .tx-sec{color:#000000;}
        .p1,.tabxplor-tab .p1{color:#000000;}
        .p2,.tabxplor-tab .p2{color:#000000;}
        .p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
        .p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline;}
        .m1,.tabxplor-tab .m1{color:#000000;font-weight:normal;font-style:italic;}
        .m2,.tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
        .m3,.tabxplor-tab .m3{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .m4,.tabxplor-tab .m4{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
        .o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
        .o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
        .o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
        .u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
        .u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
        .u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
        .u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
      }
      </style>
      
      ::: {.tabxplor-tab}
      | grp     | |            mean (sd)  |
      |:--------|-|----------------------:|
      |         | | *v*                   |
      |         | |                       |
      | A       | |     [2.5 (σ1.3)]{.m1} |
      | B       | |     [5.0       ]{.p4} |
      | C       | |                       |
      |**Total**| |  **3.0** (σ1.6)       |
      
      Shades of blue: cells ≥ the Total row **[+0.2]{.p1}**; **[+0.5]{.p3}**; **[+0.8]{.p4}** SD. Shades of yellow to red: cells ≤ the Total row **[-0.2]{.m1}**; **[-0.5]{.m3}**; **[-0.8]{.m4}** SD.
      :::

# golden display (tab_md) unchanged: totn_row_drop

    Code
      cat(tab_md(cases[[name]](), print = FALSE))
    Output
      <style>
      .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
      .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
      .tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
      .tabxplor-tab tfoot{font-size:80%;text-align:left;}
      .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
      .tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
      .tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
      .tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
      .tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab p{font-size:80%;}
      .tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab .tx-r{text-align:right;}
      .tabxplor-tab .tx-l{text-align:left;}
      .tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
      .tabxplor-tab .tx-num{white-space:nowrap;}
      .tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
      .tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
      .tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
      .tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
      .tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
      .tabxplor-tab .tx-foot{width:0;min-width:100%;}
      .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
      .tooltip-inner{max-width:none;white-space:nowrap;}
      .popover{max-width:none;}
      .popover-body,.popover-content{padding:6px;white-space:nowrap;}
      .tabxplor-tab{color:#000000;background:#ffffff;}
      .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
      .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
      .g1,.tabxplor-tab .g1{color:#9f9f9f;}
      .g2,.tabxplor-tab .g2{color:#111111;}
      .tabxplor-caption{color:#000000;}
      .tabxplor-tab .tx-sec{color:#000000;}
      .p1,.tabxplor-tab .p1{color:#02A5B3;}
      .p2,.tabxplor-tab .p2{color:#0891C9;}
      .p3,.tabxplor-tab .p3{color:#0267C7;}
      .p4,.tabxplor-tab .p4{color:#300DFD;}
      .m1,.tabxplor-tab .m1{color:#DCA331;}
      .m2,.tabxplor-tab .m2{color:#DE7C01;}
      .m3,.tabxplor-tab .m3{color:#DD5301;}
      .m4,.tabxplor-tab .m4{color:#D60103;}
      .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
      .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
      .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
      .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
      .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
      .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
      .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
      .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
      @media print {
        .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
        .tabxplor-tab{color:#000000;background:#ffffff;}
        .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
        .tabxplor-tab tbody tr:hover{background:transparent;}
        .g1,.tabxplor-tab .g1{color:#595959;}
        .g2,.tabxplor-tab .g2{color:#111111;}
        .tabxplor-caption{color:#000000;}
        .tabxplor-tab .tx-sec{color:#000000;}
        .p1,.tabxplor-tab .p1{color:#000000;}
        .p2,.tabxplor-tab .p2{color:#000000;}
        .p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
        .p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline;}
        .m1,.tabxplor-tab .m1{color:#000000;font-weight:normal;font-style:italic;}
        .m2,.tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
        .m3,.tabxplor-tab .m3{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .m4,.tabxplor-tab .m4{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
        .o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
        .o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
        .o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
        .u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
        .u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
        .u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
        .u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
      }
      </style>
      
      ::: {.tabxplor-tab}
      | g       | |    x  |    y  | |    p  |    q  |    r  | |           Total  |
      |:--------|-|------:|------:|-|------:|------:|------:|-|-----------------:|
      |         | | *h*   |       | | *k*   |       |       | |                  |
      |         | |       |       | |       |       |       | |                  |
      | A       | |  52%  |  48%  | |  29%  |  34%  |  37%  | |  100% (165-187)  |
      | B       | |  46%  |  54%  | |  34%  |  32%  |  34%  | |  100% (191-195)  |
      | C       | |  44%  |  56%  | |  39%  |  27%  |  34%  | |  100% (158-163)  |
      |**Total**| |**47%**|**53%**| |**34%**|**31%**|**35%**| |**100%** (514-545)|
      :::

# golden display (tab_md) unchanged: n_mean_color

    Code
      cat(tab_md(cases[[name]](), print = FALSE))
    Output
      <style>
      .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
      .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
      .tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
      .tabxplor-tab tfoot{font-size:80%;text-align:left;}
      .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
      .tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
      .tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
      .tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
      .tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab p{font-size:80%;}
      .tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab .tx-r{text-align:right;}
      .tabxplor-tab .tx-l{text-align:left;}
      .tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
      .tabxplor-tab .tx-num{white-space:nowrap;}
      .tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
      .tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
      .tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
      .tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
      .tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
      .tabxplor-tab .tx-foot{width:0;min-width:100%;}
      .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
      .tooltip-inner{max-width:none;white-space:nowrap;}
      .popover{max-width:none;}
      .popover-body,.popover-content{padding:6px;white-space:nowrap;}
      .tabxplor-tab{color:#000000;background:#ffffff;}
      .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
      .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
      .g1,.tabxplor-tab .g1{color:#9f9f9f;}
      .g2,.tabxplor-tab .g2{color:#111111;}
      .tabxplor-caption{color:#000000;}
      .tabxplor-tab .tx-sec{color:#000000;}
      .p1,.tabxplor-tab .p1{color:#02A5B3;}
      .p2,.tabxplor-tab .p2{color:#0891C9;}
      .p3,.tabxplor-tab .p3{color:#0267C7;}
      .p4,.tabxplor-tab .p4{color:#300DFD;}
      .m1,.tabxplor-tab .m1{color:#DCA331;}
      .m2,.tabxplor-tab .m2{color:#DE7C01;}
      .m3,.tabxplor-tab .m3{color:#DD5301;}
      .m4,.tabxplor-tab .m4{color:#D60103;}
      .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
      .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
      .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
      .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
      .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
      .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
      .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
      .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
      @media print {
        .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
        .tabxplor-tab{color:#000000;background:#ffffff;}
        .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
        .tabxplor-tab tbody tr:hover{background:transparent;}
        .g1,.tabxplor-tab .g1{color:#595959;}
        .g2,.tabxplor-tab .g2{color:#111111;}
        .tabxplor-caption{color:#000000;}
        .tabxplor-tab .tx-sec{color:#000000;}
        .p1,.tabxplor-tab .p1{color:#000000;}
        .p2,.tabxplor-tab .p2{color:#000000;}
        .p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
        .p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline;}
        .m1,.tabxplor-tab .m1{color:#000000;font-weight:normal;font-style:italic;}
        .m2,.tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
        .m3,.tabxplor-tab .m3{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .m4,.tabxplor-tab .m4{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
        .o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
        .o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
        .o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
        .u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
        .u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
        .u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
        .u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
      }
      </style>
      
      ::: {.tabxplor-tab}
      | race    | |              mean (sd)  | |            mean (sd)  |
      |:--------|-|------------------------:|-|----------------------:|
      |         | | *age*                   | | *tvhours*             |
      |         | |                         | |                       |
      | Other   | |     [39.5 (σ14.4)]{.m1} | |      2.8 (σ2.4)       |
      | Black   | |      43.9 (σ16.1)       | |     [4.2 (σ3.5)]{.p1} |
      | White   | |      48.7 (σ17.5)       | |      2.8 (σ2.3)       |
      |**Total**| |  **47.2** (σ17.3)       | |  **3.0** (σ2.6)       |
      
      Shades of blue: cells ≥ the Total row **[+0.2]{.p1}**; **[+0.5]{.p3}**; **[+0.8]{.p4}** SD. Shades of yellow to red: cells ≤ the Total row **[-0.2]{.m1}**; **[-0.5]{.m3}**; **[-0.8]{.m4}** SD.
      :::

# golden display (tab_md) unchanged: n_mean_tottab

    Code
      cat(tab_md(cases[[name]](), print = FALSE))
    Output
      <style>
      .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
      .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
      .tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
      .tabxplor-tab tfoot{font-size:80%;text-align:left;}
      .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
      .tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
      .tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
      .tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
      .tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab p{font-size:80%;}
      .tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab .tx-r{text-align:right;}
      .tabxplor-tab .tx-l{text-align:left;}
      .tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
      .tabxplor-tab .tx-num{white-space:nowrap;}
      .tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
      .tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
      .tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
      .tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
      .tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
      .tabxplor-tab .tx-foot{width:0;min-width:100%;}
      .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
      .tooltip-inner{max-width:none;white-space:nowrap;}
      .popover{max-width:none;}
      .popover-body,.popover-content{padding:6px;white-space:nowrap;}
      .tabxplor-tab{color:#000000;background:#ffffff;}
      .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
      .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
      .g1,.tabxplor-tab .g1{color:#9f9f9f;}
      .g2,.tabxplor-tab .g2{color:#111111;}
      .tabxplor-caption{color:#000000;}
      .tabxplor-tab .tx-sec{color:#000000;}
      .p1,.tabxplor-tab .p1{color:#02A5B3;}
      .p2,.tabxplor-tab .p2{color:#0891C9;}
      .p3,.tabxplor-tab .p3{color:#0267C7;}
      .p4,.tabxplor-tab .p4{color:#300DFD;}
      .m1,.tabxplor-tab .m1{color:#DCA331;}
      .m2,.tabxplor-tab .m2{color:#DE7C01;}
      .m3,.tabxplor-tab .m3{color:#DD5301;}
      .m4,.tabxplor-tab .m4{color:#D60103;}
      .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
      .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
      .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
      .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
      .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
      .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
      .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
      .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
      @media print {
        .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
        .tabxplor-tab{color:#000000;background:#ffffff;}
        .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
        .tabxplor-tab tbody tr:hover{background:transparent;}
        .g1,.tabxplor-tab .g1{color:#595959;}
        .g2,.tabxplor-tab .g2{color:#111111;}
        .tabxplor-caption{color:#000000;}
        .tabxplor-tab .tx-sec{color:#000000;}
        .p1,.tabxplor-tab .p1{color:#000000;}
        .p2,.tabxplor-tab .p2{color:#000000;}
        .p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
        .p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline;}
        .m1,.tabxplor-tab .m1{color:#000000;font-weight:normal;font-style:italic;}
        .m2,.tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
        .m3,.tabxplor-tab .m3{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .m4,.tabxplor-tab .m4{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
        .o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
        .o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
        .o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
        .u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
        .u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
        .u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
        .u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
      }
      </style>
      
      ::: {.tabxplor-tab}
      |               | race                | |              mean (sd)  | |            mean (sd)  |
      |:--------------|:--------------------|-|------------------------:|-|----------------------:|
      |               |                     | | *age*                   | | *tvhours*             |
      |               |                     | |                         | |                       |
      | No answer     | Other               | |     [34.0 (σ8.5 )]{.m3} | |     [2.0       ]{.m1} |
      |               | Black               | |     [64.0        ]{.p4} | |                       |
      |               | White               | |     [56.0 (σ15.7)]{.p3} | |      2.6 (σ1.2)       |
      |               | Total No answer     | |     [52.4 (σ16.5)]{.p1} | |      2.6 (σ1.1)       |
      |               |                     | |                         | |                       |
      | Never married | Other               | |     [30.2 (σ10.6)]{.m4} | |      2.8 (σ2.7)       |
      |               | Black               | |     [34.5 (σ12.1)]{.m3} | |     [4.2 (σ3.4)]{.p1} |
      |               | White               | |     [34.4 (σ14.3)]{.m3} | |      2.8 (σ2.6)       |
      |               | Total Never married | |     [33.9 (σ13.5)]{.m3} | |      3.1 (σ2.9)       |
      |               |                     | |                         | |                       |
      | Separated     | Other               | |     [42.5 (σ13.0)]{.m1} | |      3.3 (σ3.3)       |
      |               | Black               | |      46.2 (σ13.4)       | |     [5.1 (σ4.7)]{.p4} |
      |               | White               | |      45.6 (σ13.5)       | |      2.9 (σ2.8)       |
      |               | Total Separated     | |      45.3 (σ13.4)       | |     [3.5 (σ3.6)]{.p1} |
      |               |                     | |                         | |                       |
      | Divorced      | Other               | |      45.5 (σ11.8)       | |      3.0 (σ2.7)       |
      |               | Black               | |     [51.0 (σ12.7)]{.p1} | |     [4.3 (σ3.7)]{.p3} |
      |               | White               | |     [51.6 (σ13.2)]{.p1} | |      2.9 (σ2.4)       |
      |               | Total Divorced      | |     [51.1 (σ13.1)]{.p1} | |      3.1 (σ2.7)       |
      |               |                     | |                         | |                       |
      | Widowed       | Other               | |     [64.5 (σ14.8)]{.p4} | |     [4.2 (σ2.8)]{.p1} |
      |               | Black               | |     [67.5 (σ13.9)]{.p4} | |     [4.7 (σ3.7)]{.p3} |
      |               | White               | |     [72.8 (σ12.5)]{.p4} | |     [3.7 (σ2.7)]{.p1} |
      |               | Total Widowed       | |     [71.7 (σ13.0)]{.p4} | |     [3.9 (σ2.9)]{.p1} |
      |               |                     | |                         | |                       |
      | Married       | Other               | |     [42.2 (σ13.0)]{.m1} | |      2.5 (σ1.9)       |
      |               | Black               | |      46.4 (σ13.4)       | |     [3.8 (σ3.1)]{.p1} |
      |               | White               | |      49.7 (σ15.2)       | |      2.6 (σ2.0)       |
      |               | Total Married       | |      48.7 (σ15.1)       | |      2.7 (σ2.1)       |
      |               |                     | |                         | |                       |
      | Ensemble      | Other               | |     [39.5 (σ14.4)]{.m1} | |      2.8 (σ2.4)       |
      |               | Black               | |      43.9 (σ16.1)       | |     [4.2 (σ3.5)]{.p1} |
      |               | White               | |      48.7 (σ17.5)       | |      2.8 (σ2.3)       |
      |               |**Total Ensemble**   | |  **47.2** (σ17.3)       | |  **3.0** (σ2.6)       |
      
      Shades of blue: cells ≥ the Total row **[+0.2]{.p1}**; **[+0.5]{.p3}**; **[+0.8]{.p4}** SD. Shades of yellow to red: cells ≤ the Total row **[-0.2]{.m1}**; **[-0.5]{.m3}**; **[-0.8]{.m4}** SD.
      :::

# golden display (tab_md) unchanged: f_col_ref_lvl

    Code
      cat(tab_md(cases[[name]](), print = FALSE))
    Output
      <style>
      .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
      .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
      .tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
      .tabxplor-tab tfoot{font-size:80%;text-align:left;}
      .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
      .tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
      .tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
      .tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
      .tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab p{font-size:80%;}
      .tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab .tx-r{text-align:right;}
      .tabxplor-tab .tx-l{text-align:left;}
      .tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
      .tabxplor-tab .tx-num{white-space:nowrap;}
      .tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
      .tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
      .tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
      .tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
      .tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
      .tabxplor-tab .tx-foot{width:0;min-width:100%;}
      .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
      .tooltip-inner{max-width:none;white-space:nowrap;}
      .popover{max-width:none;}
      .popover-body,.popover-content{padding:6px;white-space:nowrap;}
      .tabxplor-tab{color:#000000;background:#ffffff;}
      .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
      .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
      .g1,.tabxplor-tab .g1{color:#9f9f9f;}
      .g2,.tabxplor-tab .g2{color:#111111;}
      .tabxplor-caption{color:#000000;}
      .tabxplor-tab .tx-sec{color:#000000;}
      .p1,.tabxplor-tab .p1{color:#02A5B3;}
      .p2,.tabxplor-tab .p2{color:#0891C9;}
      .p3,.tabxplor-tab .p3{color:#0267C7;}
      .p4,.tabxplor-tab .p4{color:#300DFD;}
      .m1,.tabxplor-tab .m1{color:#DCA331;}
      .m2,.tabxplor-tab .m2{color:#DE7C01;}
      .m3,.tabxplor-tab .m3{color:#DD5301;}
      .m4,.tabxplor-tab .m4{color:#D60103;}
      .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
      .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
      .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
      .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
      .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
      .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
      .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
      .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
      @media print {
        .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
        .tabxplor-tab{color:#000000;background:#ffffff;}
        .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
        .tabxplor-tab tbody tr:hover{background:transparent;}
        .g1,.tabxplor-tab .g1{color:#595959;}
        .g2,.tabxplor-tab .g2{color:#111111;}
        .tabxplor-caption{color:#000000;}
        .tabxplor-tab .tx-sec{color:#000000;}
        .p1,.tabxplor-tab .p1{color:#000000;}
        .p2,.tabxplor-tab .p2{color:#000000;}
        .p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
        .p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline;}
        .m1,.tabxplor-tab .m1{color:#000000;font-weight:normal;font-style:italic;}
        .m2,.tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
        .m3,.tabxplor-tab .m3{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .m4,.tabxplor-tab .m4{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
        .o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
        .o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
        .o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
        .u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
        .u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
        .u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
        .u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
      }
      </style>
      
      ::: {.tabxplor-tab}
      | marital       | |        Other  |   Black  |        White  | |        Total  |
      |:--------------|-|--------------:|---------:|--------------:|-|--------------:|
      |               | | *race*        |          |               | |               |
      |               | |               |          |               | |               |
      | No answer     | |      0%       |      0%  |      0%       | |      0%       |
      | Never married | |    [32%]{.m1} |     42%  |    [21%]{.m3} | |    [25%]{.m2} |
      | Separated     | |      6%       |      6%  |      3%       | |      3%       |
      | Divorced      | |     11%       |     16%  |     16%       | |     16%       |
      | Widowed       | |      4%       |      8%  |      9%       | |      8%       |
      | Married       | |    [48%]{.p2} |     28%  |    [51%]{.p3} | |    [47%]{.p2} |
      |**Total**      | |  **100%**     |  **100%**|  **100%**     | |  **100%**     |
      | n             | |   1 959       |   3 129  |  16 395       | |  21 483       |
      
      Shades of blue: cells ≥ the Black column **[+5]{.p1}**; **[+10]{.p2}**; **[+20]{.p3}**; **[+30]{.p4}** points. Shades of yellow to red: cells ≤ the Black column **[-5]{.m1}**; **[-10]{.m2}**; **[-20]{.m3}**; **[-30]{.m4}** points.
      :::

# golden display (tab_md) unchanged: f_col_ref_multi

    Code
      cat(tab_md(cases[[name]](), print = FALSE))
    Output
      <style>
      .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
      .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
      .tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
      .tabxplor-tab tfoot{font-size:80%;text-align:left;}
      .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
      .tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
      .tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
      .tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
      .tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab p{font-size:80%;}
      .tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab .tx-r{text-align:right;}
      .tabxplor-tab .tx-l{text-align:left;}
      .tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
      .tabxplor-tab .tx-num{white-space:nowrap;}
      .tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
      .tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
      .tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
      .tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
      .tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
      .tabxplor-tab .tx-foot{width:0;min-width:100%;}
      .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
      .tooltip-inner{max-width:none;white-space:nowrap;}
      .popover{max-width:none;}
      .popover-body,.popover-content{padding:6px;white-space:nowrap;}
      .tabxplor-tab{color:#000000;background:#ffffff;}
      .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
      .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
      .g1,.tabxplor-tab .g1{color:#9f9f9f;}
      .g2,.tabxplor-tab .g2{color:#111111;}
      .tabxplor-caption{color:#000000;}
      .tabxplor-tab .tx-sec{color:#000000;}
      .p1,.tabxplor-tab .p1{color:#02A5B3;}
      .p2,.tabxplor-tab .p2{color:#0891C9;}
      .p3,.tabxplor-tab .p3{color:#0267C7;}
      .p4,.tabxplor-tab .p4{color:#300DFD;}
      .m1,.tabxplor-tab .m1{color:#DCA331;}
      .m2,.tabxplor-tab .m2{color:#DE7C01;}
      .m3,.tabxplor-tab .m3{color:#DD5301;}
      .m4,.tabxplor-tab .m4{color:#D60103;}
      .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
      .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
      .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
      .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
      .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
      .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
      .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
      .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
      @media print {
        .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
        .tabxplor-tab{color:#000000;background:#ffffff;}
        .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
        .tabxplor-tab tbody tr:hover{background:transparent;}
        .g1,.tabxplor-tab .g1{color:#595959;}
        .g2,.tabxplor-tab .g2{color:#111111;}
        .tabxplor-caption{color:#000000;}
        .tabxplor-tab .tx-sec{color:#000000;}
        .p1,.tabxplor-tab .p1{color:#000000;}
        .p2,.tabxplor-tab .p2{color:#000000;}
        .p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
        .p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline;}
        .m1,.tabxplor-tab .m1{color:#000000;font-weight:normal;font-style:italic;}
        .m2,.tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
        .m3,.tabxplor-tab .m3{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .m4,.tabxplor-tab .m4{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
        .o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
        .o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
        .o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
        .u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
        .u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
        .u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
        .u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
      }
      </style>
      
      ::: {.tabxplor-tab}
      | marital       | |        Other  |   Black  |        White  | |    No answer  |   Don't know  |Inter-nondenominational  |Native american  |    Christian  |Orthodox-christian  | Moslem/islam  |Other eastern  |     Hinduism  |     Buddhism  |   Other  |    None  |       Jewish  |     Catholic  |   Protestant  | |        Total  |
      |:--------------|-|--------------:|---------:|--------------:|-|--------------:|--------------:|------------------------:|----------------:|--------------:|-------------------:|--------------:|--------------:|--------------:|--------------:|---------:|---------:|--------------:|--------------:|--------------:|-|--------------:|
      |               | | *race*        |          |               | | *relig*       |               |                         |                 |               |                    |               |               |               |               |          |          |               |               |               | |               |
      |               | |               |          |               | |               |               |                         |                 |               |                    |               |               |               |               |          |          |               |               |               | |               |
      | No answer     | |      0%       |      0%  |      0%       | |      4%       |      0%       |      0%                 |      0%         |      0%       |      0%            |      0%       |      0%       |      0%       |      1%       |      0%  |      0%  |      0%       |      0%       |      0%       | |      0%       |
      | Never married | |    [32%]{.m1} |     42%  |    [21%]{.m3} | |    [24%]{.m2} |     40%       |    [28%]{.m2}           |    [35%]{.m1}   |    [32%]{.m1} |    [14%]{.m3}      |    [30%]{.m2} |    [31%]{.m1} |    [31%]{.m1} |    [33%]{.m1} |     38%  |     40%  |    [24%]{.m2} |    [25%]{.m2} |    [20%]{.m3} | |    [25%]{.m2} |
      | Separated     | |      6%       |      6%  |      3%       | |      3%       |    [20%]{.p2} |      3%                 |     [9%]{.p1}   |      5%       |      2%            |      3%       |      3%       |      0%       |      3%       |      2%  |      3%  |      2%       |      4%       |      3%       | |      3%       |
      | Divorced      | |     11%       |     16%  |     16%       | |     14%       |     [7%]{.m1} |    [24%]{.p1}           |     17%         |     19%       |     15%            |     14%       |     19%       |     [3%]{.m2} |     14%       |     21%  |     16%  |     14%       |     13%       |     17%       | |     16%       |
      | Widowed       | |      4%       |      8%  |      9%       | |      8%       |      0%       |      3%                 |     [9%]{.p1}   |      3%       |      3%            |      0%       |      6%       |      0%       |      5%       |      4%  |      3%  |    [10%]{.p1} |     [8%]{.p1} |    [11%]{.p1} | |     [8%]{.p1} |
      | Married       | |    [48%]{.p2} |     28%  |    [51%]{.p3} | |    [47%]{.p2} |     33%       |    [43%]{.p1}           |    [30%]{.m1}   |     41%       |    [66%]{.p3}      |    [53%]{.p2} |     41%       |    [66%]{.p3} |    [44%]{.p1} |     35%  |     37%  |    [51%]{.p2} |    [50%]{.p2} |    [50%]{.p2} | |    [47%]{.p2} |
      |**Total**      | |  **100%**     |  **100%**|  **100%**     | |  **100%**     |  **100%**     |  **100%**               |  **100%**       |  **100%**     |  **100%**          |  **100%**     |  **100%**     |  **100%**     |  **100%**     |  **100%**|  **100%**|  **100%**     |  **100%**     |  **100%**     | |  **100%**     |
      | n             | |   1 959       |   3 129  |  16 395       | |      93       |      15       |     109                 |      23         |     689       |      95            |     104       |      32       |      71       |     147       |     224  |   3 523  |     388       |   5 124       |  10 846       | |  21 483       |
      
      Shades of blue: cells ≥ the Black column **[+5]{.p1}**; **[+10]{.p2}**; **[+20]{.p3}**; **[+30]{.p4}** points. Shades of yellow to red: cells ≤ the Black column **[-5]{.m1}**; **[-10]{.m2}**; **[-20]{.m3}**; **[-30]{.m4}** points.
      :::

# golden display (tab_md) unchanged: f_col_ref_ci

    Code
      cat(tab_md(cases[[name]](), print = FALSE))
    Output
      <style>
      .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
      .tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
      .tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
      .tabxplor-tab tfoot{font-size:80%;text-align:left;}
      .tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
      .tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
      .tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
      .tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
      .tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab p{font-size:80%;}
      .tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab .tx-r{text-align:right;}
      .tabxplor-tab .tx-l{text-align:left;}
      .tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
      .tabxplor-tab .tx-num{white-space:nowrap;}
      .tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
      .tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
      .tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
      .tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
      .tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
      .tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
      .tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
      .tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
      .tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
      .tabxplor-tab .tx-foot{width:0;min-width:100%;}
      .tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
      .tooltip-inner{max-width:none;white-space:nowrap;}
      .popover{max-width:none;}
      .popover-body,.popover-content{padding:6px;white-space:nowrap;}
      .tabxplor-tab{color:#000000;background:#ffffff;}
      .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
      .tabxplor-tab tbody tr:hover{background:#FFFCE5;}
      .g1,.tabxplor-tab .g1{color:#9f9f9f;}
      .g2,.tabxplor-tab .g2{color:#111111;}
      .tabxplor-caption{color:#000000;}
      .tabxplor-tab .tx-sec{color:#000000;}
      .p1,.tabxplor-tab .p1{color:#02A5B3;}
      .p2,.tabxplor-tab .p2{color:#0891C9;}
      .p3,.tabxplor-tab .p3{color:#0267C7;}
      .p4,.tabxplor-tab .p4{color:#300DFD;}
      .m1,.tabxplor-tab .m1{color:#DCA331;}
      .m2,.tabxplor-tab .m2{color:#DE7C01;}
      .m3,.tabxplor-tab .m3{color:#DD5301;}
      .m4,.tabxplor-tab .m4{color:#D60103;}
      .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
      .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
      .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
      .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
      .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
      .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
      .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
      .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
      @media print {
        .tabxplor-tab .tx-pill{print-color-adjust:exact;-webkit-print-color-adjust:exact;}
        .tabxplor-tab{color:#000000;background:#ffffff;}
        .tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
        .tabxplor-tab tbody tr:hover{background:transparent;}
        .g1,.tabxplor-tab .g1{color:#595959;}
        .g2,.tabxplor-tab .g2{color:#111111;}
        .tabxplor-caption{color:#000000;}
        .tabxplor-tab .tx-sec{color:#000000;}
        .p1,.tabxplor-tab .p1{color:#000000;}
        .p2,.tabxplor-tab .p2{color:#000000;}
        .p3,.tabxplor-tab .p3{color:#000000;text-decoration:underline;}
        .p4,.tabxplor-tab .p4{color:#000000;text-decoration:underline;}
        .m1,.tabxplor-tab .m1{color:#000000;font-weight:normal;font-style:italic;}
        .m2,.tabxplor-tab .m2{color:#000000;font-weight:normal;font-style:italic;}
        .m3,.tabxplor-tab .m3{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .m4,.tabxplor-tab .m4{color:#000000;font-weight:normal;font-style:italic;text-decoration:underline;}
        .o1,.tabxplor-tab .o1{background-color:#F5F5F5;}
        .o2,.tabxplor-tab .o2{background-color:#E4E4E4;}
        .o3,.tabxplor-tab .o3{background-color:#D0D0D0;}
        .o4,.tabxplor-tab .o4{background-color:#B8B8B8;}
        .u1,.tabxplor-tab .u1{background-color:#F5F5F5;}
        .u2,.tabxplor-tab .u2{background-color:#E4E4E4;}
        .u3,.tabxplor-tab .u3{background-color:#D0D0D0;}
        .u4,.tabxplor-tab .u4{background-color:#B8B8B8;}
      }
      </style>
      
      ::: {.tabxplor-tab}
      | marital       | |    Other  | Black  |     White  | |     Total  |
      |:--------------|-|----------:|-------:|-----------:|-|-----------:|
      |               | | *race*    |        |            | |            |
      |               | |           |        |            | |            |
      | No answer     | |    0%     |    0%  |     0%     | |     0%     |
      | Never married | |   32%***  |   42%  |    21%***  | |    25%***  |
      | Separated     | |    6%     |    6%  |     3%***  | |     3%***  |
      | Divorced      | |   11%***  |   16%  |    16%     | |    16%     |
      | Widowed       | |    4%***  |    8%  |     9%     | |     8%     |
      | Married       | |   48%***  |   28%  |    51%***  | |    47%***  |
      |**Total**      | |**100%**   |**100%**| **100%**   | | **100%**   |
      | n             | | 1 959     | 3 129  | 16 395     | | 21 483     |
      
      \*\*\*: significantly different from the reference category (in bold) at the 99% confidence level; \*\*: at the 95% level; \*: at the 90% level; no star: not significant.
      :::

