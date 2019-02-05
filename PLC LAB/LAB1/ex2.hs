zipLL :: ([Int],[Int])->[[Int]]
zipLL ([],[])=[]
zipLL (x:xs,[])=[x]:zipLL (xs,[])
zipLL ([],y:ys)=[y]:zipLL ([],ys)
zipLL (x:xs,y:ys) = [x,y]:zipLL (xs,ys)
          
--It is not possible to write the unzipLL in this case as we would not
--know where the unpaired elements should be put