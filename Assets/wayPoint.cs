using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class wayPoint : MonoBehaviour
{
    public GameObject nextWayPoint;
    public bool lastWayPoint;


    public void OnTriggerEnter(Collider other)
    {
        if (other.CompareTag("Player") && !lastWayPoint)
        {
            nextWayPoint.SetActive(true);
            spawner.instance.setNewWaypoint(nextWayPoint.transform);
            gameObject.SetActive(false);
        }else if(other.CompareTag("Player") && lastWayPoint)
        {
            playerTimer.instance.endLevel();

        }
    }
}
