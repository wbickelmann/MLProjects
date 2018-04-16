

libname library '\\129.177.90.101\essdata\essarbeid\ESS8\arbeidskatalog\austria\final\' ;

proc format library = library ;
   value LKNEMAT
      1 = 'Not at all likely'  
      2 = 'Not very likely'  
      3 = 'Likely'  
      4 = 'Very likely'  
      7 = 'Refusal'  
      8 = 'Don''t know'  
      9 = 'No answer' ;

proc datasets library = library ;
modify ess8csat;
   format   lknemat LKNEMAT.;
quit;
