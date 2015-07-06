
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The state of the docking station
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {dockref,
                total = 0,
                occupied = 0,
                free = 0,
                bikeRefs = []}).