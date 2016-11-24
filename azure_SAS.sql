WITH LastInWindow AS
(
    SELECT 
        MAX(Time) AS LastEventTime
    FROM 
        susieistrying TIMESTAMP BY EventProcessedUtcTime
    GROUP BY 
        TumblingWindow(hour, 12)
)
SELECT 
    susieistrying.wakeup AS wakeup
INTO wakeupoutput    
FROM
    susieistrying TIMESTAMP BY EventProcessedUtcTime 
    INNER JOIN LastInWindow
    ON DATEDIFF(hour, susieistrying, LastInWindow) BETWEEN 0 AND 12
        
SELECT 
sleep
INTO hdboutput
FROM susieistrying TIMESTAMP BY EventProcessedUtcTime
WHERE IsFirst(hour, 12) = 1
