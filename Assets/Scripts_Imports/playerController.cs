using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;

public class playerController : MonoBehaviour
{
    public LayerMask groundEnemyMask;
    public GameObject enemy;
    public GameObject spawnClick;

    private playerAnimator anim;
    private playerMotor motor;
    private Camera cam;
    public static playerController instance;
    public bool isCasting = false;
    private void Awake()
    {
        instance = this;
    }
    // Start is called before the first frame update
    void Start()
    {
        anim = GetComponent<playerAnimator>();
        motor = GetComponent<playerMotor>();
        cam = Camera.main;
    }

    // Update is called once per frame
    void Update()
    {
        if (Input.GetMouseButtonDown(0) && !isCasting)
        {
            RaycastHit hit = getMouseToWorldPos(groundEnemyMask);
            if (hit.transform.CompareTag("Enemy"))
            {
                //motor.lookAt(hit.transform);
                motor.setTarget(hit.transform);
                enemy = hit.transform.gameObject;
                anim.setTrigger("Fireball");
                isCasting = true;

            }
            else
            {
                motor.nullTarget();
                motor.setLocation(hit.point);
                Instantiate(spawnClick, hit.point + Vector3.up * .3f, Quaternion.identity).transform.forward = hit.normal;
                enemy = null;
            }
           // motor.setLocation(getMouseToWorldPos(groundEnemyMask).point);
        }
    }
    public void playerDeath()
    {
        SceneManager.LoadScene(0);
    }

    
    RaycastHit getMouseToWorldPos(int layermask)
    {
        RaycastHit hit;
        Ray ray = cam.ScreenPointToRay(Input.mousePosition);
        if (Physics.Raycast(ray, out hit, 100f,layermask))
        {
            return hit;
        }
        else
        {
            return hit;
        }
    }
}
