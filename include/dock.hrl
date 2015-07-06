
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The state of the docking station
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {dockref= empty,
                total = 0,
                occupied = 0,
                free = 0,
                bikeRefs = []}).