-record(proto, {auth=0, nav=0, cnt=0, crd=0, mberr=0, evt=0}).
-record(cup, {versionFrom = null, dict = null, size = null, step = 0, patch = null, last_sended_record=0}).

-record(state, {socket, 
				transport, 
				auth=false, 
				proto=#proto{}, 
				mnc=0, 
				mcc=0, 
				lac=0, 
				cellid=0, 
				dev_err_cnt = 0, 
				mbdev = 0, 
				dev_time = 0, 
				tracker_version = "N/A",
				session=0, 
				updated=0, 
				devid=0, 
				imei=null, 
				ticket_tarif=0, 
				money_tarif=0, 
				card_timeout=0, 
				mode=0, 
				login=0, 
				passwd=0, 
				iccid=0, 

%% Services  				
				nav_service=0, 
				cnt_service=0, 
				crd_service=0, 
				egts_service = 0,
				output_h = 0,
				output_p = 0,
%% Answers 							 
				type=0, 
				complete=0, 
				crd_decoded=0, 
				egts_packets=[], 
%% CUP part			
				cup=#cup{}	
				}).

-record(crd, {  time_curr=null,
                                time_prev=null,
                                tid_curr = null,
                                tid_prev = null,
                                uid = null,
                                suls_id = null,
                                op_type = null,
                                balance_curr = null,
                                balance_prev = null,
                                entries_curr = null,
                                entries_prev = null,
                                card_dump = null
}).


-record(session, {devID=null, atime, imei=null}).
-record(session_state, {timer, sessions}).

-record(filter, {devID=null, imei=null}).


-record(events, {type, time, value}).


-record(event, {type = null, time=null, value=null, lat=null, lon=null, speed=null, course=null, status=null, gps_sats=null, gl_sats=null, rssi=null, movement=null, mt285=null}).

-record(cnt, {time=null, a=null, b=null, irs_a=null, irs_b=null, serial=null}).

