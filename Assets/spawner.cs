using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class spawner : MonoBehaviour
{
    public GameObject Golem;
    public Transform currentWayPoint;
    public float spawnRadius = 25;
    int wayPointNum = 0;
    public int initSpawn = 10;
    public int spawnincreaseFactor = 5;
    public static spawner instance;
    private void Awake()
    {
        instance = this;
    }
    // Start is called before the first frame update
    void Start()
    {
        spawn();
    }

    // Update is called once per frame
    void Update()
    {
        
    }
    public void setNewWaypoint(Transform t)
    {
        currentWayPoint = t;
        wayPointNum++;
        spawn();
    }
    public void spawn()
    {
        int tot = initSpawn + spawnincreaseFactor * wayPointNum;
        for (int i = 0; i < initSpawn; i++)
        {
            Vector3 pos = currentWayPoint.position + makeDisk(spawnRadius + wayPointNum, 5, 0);
            Instantiate(Golem, pos, Quaternion.identity);
        }
    }
    Vector3 makeDisk(float radius, float random, float yVal)
    {
        Vector2 disk = Random.insideUnitCircle * (radius + Random.Range(-random, random));
        Vector3 finalDisk = new Vector3(disk.x, yVal, disk.y);
        return finalDisk;
    }
}
