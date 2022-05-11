
function call = BreakVideoCall(vid, startt, endt, step, savetodir)

vidObj = VideoReader(vid);
duration = vidObj.Duration;

 if endt == 99999 
    endt = duration;
 end
      
 if endt > duration
    endt = duration; 
 end
 
 if endt - startt > 30
    k = ceil((endt - startt)/30);
    i = 1;
    endt_user = endt;
    while i <= k
    startt = startt;    
    endt =  min(startt + 30, endt_user);
    imgs = getVideoFrames(vid, startt, endt, step, savetodir); 
    i = i + 1;
    startt = endt;
    end
    
 else 
    imgs = getVideoFrames(vid, startt, endt, step, savetodir); 
end

end



